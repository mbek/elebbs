unit MusicFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MPlayer, StdCtrls, ExtCtrls;

type
  TMusicForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Timer1: TTimer;
    MediaPlayer1: TMediaPlayer;
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PageAborted: Char;
  end;

var
  MusicForm: TMusicForm;

implementation

{$R *.DFM}

procedure TMusicForm.Timer1Timer(Sender: TObject);
begin
   PageAborted := #00; { Expired }
   MusicForm.Timer1.Enabled := false;
   if MediaPlayer1.mode=mpPlaying then
     MediaPlayer1.Stop;
   Close;
end;

procedure TMusicForm.Button1Click(Sender: TObject);
begin
   PageAborted := 'C'; { Chat }
   MusicForm.Timer1.Enabled := false;
   if MediaPlayer1.mode=mpPlaying then
     MediaPlayer1.Stop;
   Close;
end;

procedure TMusicForm.Button2Click(Sender: TObject);
begin
   PageAborted := 'A'; { Abort }
   MusicForm.Timer1.Enabled := false;
   if MediaPlayer1.mode=mpPlaying then
     MediaPlayer1.Stop;
   Close;
end;

end.
