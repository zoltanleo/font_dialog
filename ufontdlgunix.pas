unit ufontdlgunix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ColorBox, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics,
  Dialogs;

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
    splFontDlgUnixHorz: TSplitter;
    splFontDlgUnixVert: TSplitter;
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
  CloseAction:= caFree;
end;

procedure TfrmFontDlgUnix.FormCreate(Sender: TObject);
const
  SelfMinValue = 200;
begin
  with Self do
  begin
    ModalResult:= mrCancel;
    AutoScroll:= True;
    KeyPreview:= True;
    BorderIcons:= [biSystemMenu];
    Constraints.MinHeight:= SelfMinValue * 2;
    Constraints.MinWidth:= SelfMinValue * 3;
  end;



  gbTemplate.Constraints.MinHeight:= Self.Canvas.TextHeight('W') * 3;
end;

procedure TfrmFontDlgUnix.SetSelfFont(AValue: TFont);
begin
  FSelfFont.Assign(AValue);
end;

end.

