{
Headesetcheck
Copyright (C) 2020  Marco Caselli <marcocas@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
unit dmrec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazDyn_PortAudio, process, MouseAndKeyInput, LCLType, ctypes, Forms, ExtCtrls, Menus, LCLIntf;

type

  { Tdm }

  Tdm = class(TDataModule)
    N1: TMenuItem;
    mnuCommand: TMenuItem;
    mnuKey: TMenuItem;
    mnuQuit: TMenuItem;
    PopupMenu1: TPopupMenu;
    TrayIcon1: TTrayIcon;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure mnuKeyClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
  private
    PAParam: PaStreamParameters;
    padevice: integer;
    Handlest: PPaStream;
  public
    procedure Async(Data: PtrInt);

  end;

var
  dm: Tdm;

implementation

uses Dialogs;

const
  // Find the rigth shared library for current platform
{$IFDEF LINUX}
  PORTAUDIOLIB = 'libportaudio.so.2';
{$ENDIF LINUX}
{$IFDEF WINDOWS}
  {$ifdef CPU64}
  PORTAUDIOLIB = 'libportaudio-64.dll';
  {$endif}
  {$ifdef CPU32}
  PORTAUDIOLIB = 'libportaudio-32.dll';
  {$endif}
{$ENDIF WINDOWS}
{$IFDEF DARWIN}
  PORTAUDIOLIB = 'LibPortaudio.dylib';
{$ENDIF DARWIN}

const
  SAMPLE_RATE = 1000; // # Sample rate for our input stream
  BLOCK_SIZE = 200; //# Number of samples before we trigger a processing callback
  PRESS_SECONDS = 0.2; //# Number of seconds button should be held to register press
  PRESS_SAMPLE_THRESHOLD = 60000; //# Signal amplitude to register as a button click
  BLOCKS_TO_PRESS = (SAMPLE_RATE / BLOCK_SIZE) * PRESS_SECONDS;

  /// some useful key code for multimedia control
  //  VK_VOLUME_MUTE = 173;
  //  VK_VOLUME_DOWN = 174;
  //  VK_VOLUME_UP = 175;
  VK_MEDIA_NEXT_TRACK = 176;
  VK_MEDIA_PREV_TRACK = 177;
  //  VK_MEDIA_STOP = 178;
  VK_MEDIA_PLAY_PAUSE = 179;

var
  is_held: boolean;
  time_pressed: integer;

{$R *.lfm}

{ Tdm }
function StreamCallback(input: Pointer; output: Pointer; frameCount: CULong; timeInfo: PPaStreamCallbackTimeInfo; statusFlags: PaStreamCallbackFlags; userData: Pointer): CInt32; cdecl;
var
  Mean: integer;
  Buf: pcuint16;
  Acc: int64;
  i, j: integer;
begin
  acc := 0;
  buf := input;
  for i := 0 to (frameCount) - 1 do
  begin
    Acc := acc + Buf^;
    Inc(buf);
  end;
  Mean := Acc div (frameCount);
  if Mean < PRESS_SAMPLE_THRESHOLD then
  begin
    Inc(time_pressed);
    if (time_pressed > BLOCKS_TO_PRESS) and not is_held then
    begin
      is_held := True;
      if GetKeyState(VK_SHIFT) < 0 then  // is shift down
        Application.QueueAsyncCall(@dm.Async, VK_MEDIA_NEXT_TRACK)
      else
      if GetKeyState(VK_CONTROL) < 0 then  // is ctrl-down
        Application.QueueAsyncCall(@dm.Async, VK_MEDIA_PREV_TRACK)
      else
        Application.QueueAsyncCall(@dm.Async, VK_MEDIA_PLAY_PAUSE);

    end;
  end
  else
  begin
    is_held := False;
    time_pressed := 0;
  end;
  Result := 0;
end;

procedure Tdm.DataModuleCreate(Sender: TObject);
var
  err: PaError;
begin
  Handlest := nil;
  if not Pa_Load(PORTAUDIOLIB) then
  begin
    MessageDlg('Cannot load Portaudio library', mtError, [mbAbort], 0);
    Halt(1);
  end;
  Err := Pa_Initialize();
  if Err <> paError(paNoError) then
  begin
    MessageDlg('Portaudio error: ' + Pa_GetErrorText(err), mtError, [mbAbort], 0);
    Halt(2);
  end;
  padevice := 0;
  PAParam.HostApiSpecificStreamInfo := nil;
  PAParam.device := padevice;
  PAParam.SuggestedLatency := 0.0;
  PAParam.SampleFormat := paInt16;
  PAParam.channelCount := 1;
  err := Pa_OpenStream(@HandleSt, @PAParam, nil, SAMPLE_RATE, BLOCK_SIZE, paClipOff, PPaStreamCallback(@StreamCallback), nil);
  if Err <> paError(paNoError) then
  begin
    MessageDlg('Portaudio error: ' + Pa_GetErrorText(err), mtError, [mbAbort], 0);
    Halt(3);
  end;
  is_held := True;
  time_pressed := 0;
  Err := Pa_StartStream(Handlest);
  if Err <> paError(paNoError) then
  begin
    MessageDlg('Portaudio error: ' + Pa_GetErrorText(err), mtError, [mbAbort], 0);
    Halt(4);
  end;
end;

procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(Handlest) then
    Pa_StopStream(Handlest);
  Pa_Unload();
end;

procedure Tdm.mnuKeyClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
end;

procedure Tdm.mnuQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure Tdm.Async(Data: PtrInt);
var
  dummy: string;
begin
  // send virtual key press to O.S.
  if mnuKey.Checked then
    KeyInput.Press(Data);

  // or you can easily run an external application
  if mnuCommand.Checked then
    case Data of
      VK_MEDIA_NEXT_TRACK: RunCommand('C:\source\ovoplayer\bin\win32\ovoplayerctrl.exe', ['--next'], dummy);
      VK_MEDIA_PREV_TRACK: RunCommand('C:\source\ovoplayer\bin\win32\ovoplayerctrl.exe', ['--previous'], dummy);
      VK_MEDIA_PLAY_PAUSE: RunCommand('C:\source\ovoplayer\bin\win32\ovoplayerctrl.exe', ['--playpause'], dummy);
    end;
end;

end.






