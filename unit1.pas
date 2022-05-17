unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLType, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(Label1.Font);
  if FontDialog1.Execute then
  begin
    Label1.Font.Assign(FontDialog1.Font);
    StaticText1.Font.Assign(FontDialog1.Font);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aLogFont: TLogFont;
begin
  //uses LCLType
  //see also https://docs.microsoft.com/en-us/windows/win32/api/wingdi/ns-wingdi-logfonta
  with aLogFont do
  begin
    lfCharSet:= Label1.Font.CharSet;
    lfHeight:= Label1.Font.Height;

    if (fsBold in Label1.Font.Style)
      then lfWeight:= FW_BOLD
      else lfWeight:= FW_NORMAL;

    lfItalic:= Byte(fsItalic in Label1.Font.Style);
    lfStrikeOut:= Byte(fsStrikeOut in Label1.Font.Style);
    lfUnderline:= Byte(fsUnderline in Label1.Font.Style);

    lfOrientation:= Label1.Font.Orientation;
    lfOutPrecision:= OUT_DEFAULT_PRECIS;
    lfClipPrecision:= CLIP_DEFAULT_PRECIS;
    lfQuality:= Byte(Label1.Font.Quality);
    lfPitchAndFamily:= Byte(Label1.Font.Pitch);
    lfFaceName:= Label1.Font.Name;
  end;
  FontDialog1.Font.Assign(aLogFont);

  if FontDialog1.Execute then
  begin
    with aLogFont do
    begin
      lfCharSet:= FontDialog1.Font.CharSet;
      lfHeight:= FontDialog1.Font.Height;

      if (fsBold in FontDialog1.Font.Style)
        then lfWeight:= FW_BOLD
        else lfWeight:= FW_NORMAL;

      lfItalic:= Byte(fsItalic in FontDialog1.Font.Style);
      lfStrikeOut:= Byte(fsStrikeOut in FontDialog1.Font.Style);
      lfUnderline:= Byte(fsUnderline in FontDialog1.Font.Style);

      lfOrientation:= FontDialog1.Font.Orientation;
      lfOutPrecision:= OUT_DEFAULT_PRECIS;
      lfClipPrecision:= CLIP_DEFAULT_PRECIS;
      lfQuality:= Byte(FontDialog1.Font.Quality);
      lfPitchAndFamily:= Byte(FontDialog1.Font.Pitch);
      lfFaceName:= FontDialog1.Font.Name;
    end;

    Label1.Font.Assign(aLogFont);
    StaticText1.Font.Assign(aLogFont);
  end;
end;

end.

