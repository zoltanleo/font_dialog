unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , ColorBox
  , LCLType
  , StdCtrls
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ufontdialogex;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    chbBold: TCheckBox;
    chbItalic: TCheckBox;
    chbStrikeOut: TCheckBox;
    chbUnderline: TCheckBox;
    ColorBox1: TColorBox;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chbBoldChange(Sender: TObject);
    procedure chbItalicChange(Sender: TObject);
    procedure chbStrikeOutChange(Sender: TObject);
    procedure chbUnderlineChange(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TForm1.Button3Click(Sender: TObject);
var
  tmpFrm: TfrmFontDialogEx = nil;
begin
  tmpFrm:= TfrmFontDialogEx.Create(Self);

  try
    tmpFrm.SelfFont.Assign(Label1.Font);
    tmpFrm.ShowModal;

    if (tmpFrm.ModalResult = mrOK) then
    begin
      Label1.Font.Assign(tmpFrm.SelfFont);
      StaticText1.Font.Assign(tmpFrm.SelfFont);
      chbBold.Checked:= (fsBold in tmpFrm.SelfFont.Style);
      chbItalic.Checked:= (fsItalic in tmpFrm.SelfFont.Style);
      chbStrikeOut.Checked:= (fsStrikeOut in tmpFrm.SelfFont.Style);
      chbUnderline.Checked:= (fsUnderline in tmpFrm.SelfFont.Style);
      ColorBox1.Selected:= tmpFrm.SelfFont.Color;
    end;
  finally
    FreeAndNil(tmpFrm);
  end;
end;

procedure TForm1.chbBoldChange(Sender: TObject);
begin
  if chbBold.Checked
  then
    begin
      Label1.Font.Style:= StaticText1.Font.Style + [fsBold];
      StaticText1.Font.Style:= StaticText1.Font.Style + [fsBold];
    end
  else
    begin
      Label1.Font.Style:= StaticText1.Font.Style - [fsBold];
      StaticText1.Font.Style:= StaticText1.Font.Style - [fsBold];
    end;
end;

procedure TForm1.chbItalicChange(Sender: TObject);
begin
  if chbItalic.Checked
  then
    begin
      Label1.Font.Style:= StaticText1.Font.Style + [fsItalic];
      StaticText1.Font.Style:= StaticText1.Font.Style + [fsItalic];
    end
  else
    begin
      Label1.Font.Style:= StaticText1.Font.Style - [fsItalic];
      StaticText1.Font.Style:= StaticText1.Font.Style - [fsItalic];
    end
end;

procedure TForm1.chbStrikeOutChange(Sender: TObject);
begin
  if chbStrikeOut.Checked
  then
    begin
      Label1.Font.Style:= StaticText1.Font.Style + [fsStrikeOut];
      StaticText1.Font.Style:= StaticText1.Font.Style + [fsStrikeOut];
    end
  else
    begin
      Label1.Font.Style:= StaticText1.Font.Style - [fsStrikeOut];
      StaticText1.Font.Style:= StaticText1.Font.Style - [fsStrikeOut];
    end

end;

procedure TForm1.chbUnderlineChange(Sender: TObject);
begin
  if chbUnderline.Checked
    then
      begin
        Label1.Font.Style:= StaticText1.Font.Style + [fsUnderline];
        StaticText1.Font.Style:= StaticText1.Font.Style + [fsUnderline];
      end
    else
      begin
        Label1.Font.Style:= StaticText1.Font.Style - [fsUnderline];
        StaticText1.Font.Style:= StaticText1.Font.Style - [fsUnderline];
      end;
end;

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  Label1.Font.Color:= ColorBox1.Selected;
  StaticText1.Font.Color:= ColorBox1.Selected;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Font.Size:= 15;
  StaticText1.Font.Size:= 15;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  chbBoldChange(Self);
  chbItalicChange(Self);
  chbStrikeOutChange(Self);
  chbUnderlineChange(Self);
  ColorBox1.Selected:= clWindowText;
end;

end.

