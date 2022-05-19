unit ufontdlgunix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ColorBox, ExtCtrls, LCLType, StdCtrls, SysUtils, Forms, Controls,
  Graphics, Dialogs;

type

  { TfrmFontDlgUnix }

  TfrmFontDlgUnix = class(TForm)
    btnSelect: TButton;
    btnCancel: TButton;
    chbFontStyleStrike: TCheckBox;
    chbFontStyleUnderline: TCheckBox;
    ColorBox1: TColorBox;
    cbbFontCharset: TComboBox;
    edtFontSize: TEdit;
    gbEffects: TGroupBox;
    gbTemplate: TGroupBox;
    lblTemplate: TLabel;
    lblFontCharset: TLabel;
    lblFontColor: TLabel;
    lblFontSize: TLabel;
    lblFamily: TLabel;
    lblTypeFace: TLabel;
    lbxFontFamily: TListBox;
    lbxTypeFace: TListBox;
    lbxFontSize: TListBox;
    procedure edtFontSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FSelfFont: TFont;
    procedure SetSelfFont(AValue: TFont);
  public
    property SelfFont: TFont read FSelfFont write SetSelfFont;
  end;

var
  frmFontDlgUnix: TfrmFontDlgUnix;

implementation

{$R *.lfm}

{ TfrmFontDlgUnix }

procedure TfrmFontDlgUnix.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FSelfFont);
  CloseAction:= caFree;
end;

procedure TfrmFontDlgUnix.edtFontSizeKeyPress(Sender: TObject; var Key: char);
//const
//  dig_arr = Array['0','1'] of char;
begin
  //Self.Caption:= IntToStr(ord(Key));
  //if (((ord(Key) < VK_0) or (ord(Key) > VK_9)) or (Ord(Key) = VK_BACK)) then Key:= #0;
end;

procedure TfrmFontDlgUnix.FormCreate(Sender: TObject);
begin
  with Self do
  begin
    ModalResult:= mrCancel;
    KeyPreview:= True;
    BorderStyle:= bsDialog;
  end;

  gbEffects.Constraints.MinWidth:= Self.Canvas.TextHeight('W') * 12;
  gbEffects.AutoSize:= True;

  FSelfFont:= TFont.Create;
  edtFontSize.Clear;

end;

procedure TfrmFontDlgUnix.SetSelfFont(AValue: TFont);
begin
  FSelfFont.Assign(AValue);
end;

end.

