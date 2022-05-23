unit ufontdlgunix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , ColorBox
  , ExtCtrls
  , LCLIntf, LCLType
  , StdCtrls
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  ;

type

  { TfrmFontDlgUnix }

  TfrmFontDlgUnix = class(TForm)
    btnSelect: TButton;
    btnCancel: TButton;
    chbFontStyleStrike: TCheckBox;
    chbFontStyleUnderline: TCheckBox;
    clboxFontColor: TColorBox;
    cbbFontCharset: TComboBox;
    cbbFontPitch: TComboBox;
    edtFontSize: TEdit;
    gbEffects: TGroupBox;
    gbTemplate: TGroupBox;
    lblFontPitch: TLabel;
    lblTemplate: TLabel;
    lblFontCharset: TLabel;
    lblFontColor: TLabel;
    lblFontSize: TLabel;
    lblFamily: TLabel;
    lblStyles: TLabel;
    lbxFontFamily: TListBox;
    lbxStyles: TListBox;
    lbxFontSize: TListBox;
    procedure cbbFontCharsetChange(Sender: TObject);
    procedure cbbFontPitchChange(Sender: TObject);
    procedure chbFontStyleStrikeChange(Sender: TObject);
    procedure chbFontStyleUnderlineChange(Sender: TObject);
    procedure clboxFontColorChange(Sender: TObject);
    procedure edtFontSizeEditingDone(Sender: TObject);
    procedure edtFontSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxFontFamilyClick(Sender: TObject);
    procedure lbxFontSizeClick(Sender: TObject);
    procedure lbxStylesClick(Sender: TObject);
  private
    FBtnCancelCaption: String;//button "Cancel" caption
    FBtnSelectCaption: String;//button "Select" caption
    FFontSizeMaxValue: PtrUInt;//maximum allowed font size
    FFontSizeMinValue: PtrUInt;//minimum allowed font size
    FSelfFont: TFont;//font instance to use inside the component
    FTemplateCaption: String;//template caption
    FTitleCaption: String;//font dialoge caption
    function  GetCharSet: byte;
    function  GetPitch: Byte;
    procedure SetSelfFont(AValue: TFont);
    procedure LoadFontList;
    procedure SetCharsetList;
    procedure SetPitchList;
    procedure SetStylesList;
    procedure ApplyTemplateTextChanges;
  public
    property SelfFont: TFont read FSelfFont write SetSelfFont;//font instance to use inside the component
    property FontSizeMinValue: PtrUInt read FFontSizeMinValue write FFontSizeMinValue;//minimum allowed font size
    property FontSizeMaxValue: PtrUInt read FFontSizeMaxValue write FFontSizeMaxValue;//maximum allowed font size
    property TemplateCaption: String read FTemplateCaption write FTemplateCaption;//template caption
    property TitleCaption: String read FTitleCaption write FTitleCaption;//font dialoge caption
    property BtnSelectCaption: String read FBtnSelectCaption write FBtnSelectCaption;//button "Select" caption
    property BtnCancelCaption: String read FBtnCancelCaption write FBtnCancelCaption;//button "Cancel" caption
  end;

var
  frmFontDlgUnix: TfrmFontDlgUnix;

implementation

{$R *.lfm}
var
  SelectedFontCharset: Byte;

function EnumFontsNoDups(
  var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx;
  FontType: Longint;
  Data: LParam):LongInt; stdcall;
var
  L: TStringList;
  S: String;
begin
  L := TStringList(PtrInt(Data));
  S := LogFont.elfLogFont.lfFaceName;
  if (L.IndexOf(S) < 0) then
    L.Add(S);
  result:= 1;
end;

function EnumFamilyFonts(
  var eLogFont: TEnumLogFontEx;
  var Metric:TNewTextMetricEx;
  FontType:longint;
  Data:LParam):longint; stdcall;
var
  s: string;
  n: integer;
  sl: TStringList = nil;
begin
  SelectedFontCharset:= eLogFont.elfLogFont.lfCharSet;

  sl:= TStringList(PtrInt(Data));
  // collect styles
  s:= eLogFont.elfStyle;
  if (sl.IndexOf(s) < 0) then
  begin
    // encode bold, italic
    n:= eLogFont.elfLogFont.lfItalic;

    if (eLogFont.elfLogFont.lfWeight > FW_MEDIUM)
      then n:= (n or 2)
      else
        if (n > 0)
          then n:= (n xor 2);

    sl.AddObject(eLogFont.elfStyle, TObject(PtrInt(n)));
  end;

  result := 1;
end;

{ TfrmFontDlgUnix }

procedure TfrmFontDlgUnix.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FSelfFont);
  CloseAction:= caFree;
end;

procedure TfrmFontDlgUnix.edtFontSizeKeyPress(Sender: TObject; var Key: char);
begin
  Self.Caption:= IntToStr(ord(Key));
  if ((Ord(Key) <> VK_BACK) and ((ord(Key) < VK_0) or (ord(Key) > VK_9))) then Key:= #0;
end;

procedure TfrmFontDlgUnix.edtFontSizeEditingDone(Sender: TObject);
begin
  if (StrToInt(edtFontSize.Text) < FontSizeMinValue)
    then edtFontSize.Text:= IntToStr(FontSizeMinValue);

  if (StrToInt(edtFontSize.Text) > FontSizeMaxValue)
    then edtFontSize.Text:= IntToStr(FontSizeMaxValue);

  if (lbxFontSize.Items.IndexOf(edtFontSize.Text) >= 0)
    then lbxFontSize.ItemIndex:= lbxFontSize.Items.IndexOf(edtFontSize.Text);

  ApplyTemplateTextChanges;
end;

procedure TfrmFontDlgUnix.cbbFontCharsetChange(Sender: TObject);
begin
  LoadFontList;
  lbxFontFamilyClick(Sender);//lbxStylesClick and ApplyTemplateTextChanges inside
end;

procedure TfrmFontDlgUnix.cbbFontPitchChange(Sender: TObject);
begin
  LoadFontList;
  lbxFontFamilyClick(Sender);//lbxStylesClick and ApplyTemplateTextChanges inside
end;

procedure TfrmFontDlgUnix.chbFontStyleStrikeChange(Sender: TObject);
begin
  ApplyTemplateTextChanges;
end;

procedure TfrmFontDlgUnix.chbFontStyleUnderlineChange(Sender: TObject);
begin
  ApplyTemplateTextChanges;
end;

procedure TfrmFontDlgUnix.clboxFontColorChange(Sender: TObject);
begin
  ApplyTemplateTextChanges;
end;

procedure TfrmFontDlgUnix.FormCreate(Sender: TObject);
var
  i: PtrUInt = 0;
begin
  with Self do
  begin
    ModalResult:= mrCancel;
    KeyPreview:= True;
    BorderStyle:= bsDialog;
    FFontSizeMinValue:= 4;
    FFontSizeMaxValue:= 100;
    FTemplateCaption:= 'АБВГД абвгд ABCDE abcde 12345';
    FBtnCancelCaption:= 'Cancel';
    FBtnSelectCaption:= 'Select';
  end;

  FSelfFont:= TFont.Create;
  edtFontSize.Clear;
  edtFontSize.MaxLength:= 3;

  SetCharsetList;
  cbbFontCharset.ItemIndex:= GetCharSet;//DEFAULT_CHARSET
  SetPitchList;
  cbbFontPitch.ItemIndex:= GetPitch;//DEFAULT_PITCH

  //fill FonstSize listbox
  lbxFontSize.Clear;
  for i:= FontSizeMinValue to FontSizeMaxValue do
  begin
    if (i <= 12)
      then lbxFontSize.Items.Add(IntToStr(i))
      else
        if ((i > 12) and (i <= 28))
          then
            begin
              if (i mod 2 = 0)
                then lbxFontSize.Items.Add(IntToStr(i));
            end
          else
            if ((i mod 12 = 0) and (i <> 60) and (i <= 72))
              then lbxFontSize.Items.Add(IntToStr(i));
  end;
end;

procedure TfrmFontDlgUnix.FormShow(Sender: TObject);
begin
  if (LowerCase(FSelfFont.Name) = 'default')
  then //load default font setting
    begin

    end
  else
    begin

    end;

  //fill Font List according settings
  LoadFontList;

  if (LowerCase(FSelfFont.Name) = 'default')
  then //load default font setting
    begin
      lbxFontFamily.ItemIndex:= lbxFontFamily.Items.IndexOf(Screen.SystemFont.Name);
      if (lbxFontSize.Items.IndexOf(IntToStr(Screen.SystemFont.Size)) >= 0) then
      begin
        lbxFontSize.ItemIndex:= lbxFontSize.Items.IndexOf(IntToStr(Screen.SystemFont.Size));
        edtFontSize.Text:= IntToStr(Screen.SystemFont.Size);

      end;
    end
  else //load another font setting
    begin
      lbxFontFamily.ItemIndex:= lbxFontFamily.Items.IndexOf(FSelfFont.Name);
      if (lbxFontSize.Items.IndexOf(IntToStr(FSelfFont.Size)) >= 0) then
      begin
        lbxFontSize.ItemIndex:= lbxFontSize.Items.IndexOf(IntToStr(FSelfFont.Size));
        edtFontSize.Text:= IntToStr(FSelfFont.Size);
      end;
    end;

  lbxFontFamilyClick(Sender);
end;

procedure TfrmFontDlgUnix.lbxFontFamilyClick(Sender: TObject);
begin
  SetStylesList;
  lbxStylesClick(Sender);//ApplyTemplateTextChanges inside
end;

procedure TfrmFontDlgUnix.lbxFontSizeClick(Sender: TObject);
begin
  edtFontSize.Text:= lbxFontSize.Items[lbxFontSize.ItemIndex];
  ApplyTemplateTextChanges;
end;

procedure TfrmFontDlgUnix.lbxStylesClick(Sender: TObject);
begin
  ApplyTemplateTextChanges;
end;

function TfrmFontDlgUnix.GetCharSet: byte;
begin
  if (cbbFontCharset.Itemindex < 0)
    then Result := DEFAULT_CHARSET
    else Result := Byte(PtrInt(cbbFontCharset.Items.Objects[cbbFontCharset.ItemIndex]));
end;

function TfrmFontDlgUnix.GetPitch: Byte;
begin
  if (cbbFontPitch.ItemIndex < 0)
    then Result:= DEFAULT_PITCH
    else Result:= Byte(PtrInt(cbbFontPitch.Items.Objects[cbbFontPitch.ItemIndex]));
end;

procedure TfrmFontDlgUnix.SetSelfFont(AValue: TFont);
begin
  FSelfFont.Assign(AValue);
end;

procedure TfrmFontDlgUnix.LoadFontList;
var
  DC: HDC;
  aLogFont: TLogFont;
  SL: TStringList = nil;
  //i: Integer;
  OldFontName: String = '';
begin
  if (lbxFontFamily.Items.Count > 0)
    then OldFontName:= lbxFontFamily.Items[lbxFontFamily.ItemIndex];

  aLogFont.lfCharSet:= GetCharSet; //find current font charset
  aLogFont.lfFaceName:= ''; //find ALL system fonts
  aLogFont.lfPitchAndFamily:= GetPitch;//get current font pitch

  lbxStyles.Clear;
  SL:= TStringList.Create;
  DC:= GetDC(0);

  try
    //fill list of system fonts
    EnumFontFamiliesEX(DC, @aLogFont, @EnumFontsNoDups, PtrInt(SL), 0);
    SL.Sort;
    lbxFontFamily.Clear;
    lbxFontFamily.Items.Assign(SL);

    if (lbxFontFamily.Items.IndexOf(OldFontName) >= 0)
      then lbxFontFamily.ItemIndex:= lbxFontFamily.Items.IndexOf(OldFontName)
      else lbxFontFamily.ItemIndex:= 0;
  finally
    ReleaseDC(0,DC);
    FreeAndNil(SL);
  end;
end;

procedure TfrmFontDlgUnix.SetCharsetList;
  procedure AddCharSet(Charset: PtrInt);
  begin
    cbbFontCharset.Items.AddObject(CharSetToString(CharSet), TObject(PtrInt(Charset)));
  end;
begin
  cbbFontCharset.Items.Clear;
  AddCharSet(ANSI_CHARSET);
  AddCharSet(DEFAULT_CHARSET);
  AddCharSet(SYMBOL_CHARSET);
  AddCharSet(MAC_CHARSET);
  AddCharSet(SHIFTJIS_CHARSET);
  AddCharSet(HANGEUL_CHARSET);
  AddCharSet(JOHAB_CHARSET);
  AddCharSet(GB2312_CHARSET);
  AddCharSet(CHINESEBIG5_CHARSET);
  AddCharSet(GREEK_CHARSET);
  AddCharSet(TURKISH_CHARSET);
  AddCharSet(VIETNAMESE_CHARSET);
  AddCharSet(HEBREW_CHARSET);
  AddCharSet(ARABIC_CHARSET);
  AddCharSet(BALTIC_CHARSET);
  AddCharSet(RUSSIAN_CHARSET);
  AddCharSet(THAI_CHARSET);
  AddCharSet(EASTEUROPE_CHARSET);
  AddCharSet(OEM_CHARSET);
  AddCharSet(FCS_ISO_10646_1);
  AddCharSet(FCS_ISO_8859_1);
  AddCharSet(FCS_ISO_8859_2);
  AddCharSet(FCS_ISO_8859_3);
  AddCharSet(FCS_ISO_8859_4);
  AddCharSet(FCS_ISO_8859_5);
  AddCharSet(FCS_ISO_8859_6);
  AddCharSet(FCS_ISO_8859_7);
  AddCharSet(FCS_ISO_8859_8);
  AddCharSet(FCS_ISO_8859_9);
  AddCharSet(FCS_ISO_8859_10);
  AddCharSet(FCS_ISO_8859_15);
end;

procedure TfrmFontDlgUnix.SetPitchList;
begin
  cbbFontPitch.Items.Clear;
  cbbFontPitch.Items.AddObject('DEFAULT_PITCH',TObject(PtrInt(DEFAULT_PITCH)));
  cbbFontPitch.Items.AddObject('FIXED_PITCH',TObject(PtrInt(FIXED_PITCH)));
  cbbFontPitch.Items.AddObject('VARIABLE_PITCH',TObject(PtrInt(VARIABLE_PITCH)));
  cbbFontPitch.Items.AddObject('MONO_FONT',TObject(PtrInt(MONO_FONT)));
end;

procedure TfrmFontDlgUnix.SetStylesList;
var
  LCharset: TStringList = nil;
  LStyles: TStringList = nil;
  dc: HDC;
  Lf: TLogFont;
begin
  DC:= GetDC(0);
  LStyles:= TStringList.Create;
  try
    // enumerate fonts
    Lf.lfFaceName := lbxFontFamily.Items[lbxFontFamily.ItemIndex];
    Lf.lfCharSet := GetCharSet;
    Lf.lfPitchAndFamily := GetPitch;
    EnumFontFamiliesEX(DC, @Lf, @EnumFamilyFonts, PtrInt(LStyles), 0);

    //Self.Caption:= IntToStr(PtrInt(SelectedFontCharset));
    //cbbFontCharset.ItemIndex:= PtrInt(SelectedFontCharset);

    // fill styles listbox
    LStyles.Sort;
    lbxStyles.Items.Assign(LStyles);

    if (lbxFontFamily.Items.Count > 0) then lbxStyles.ItemIndex:= 0;
  finally
    FreeAndNil(LStyles);
    ReleaseDC(0, DC);
  end;
end;

procedure TfrmFontDlgUnix.ApplyTemplateTextChanges;
var
  i: PtrInt = 0;
begin
  with lblTemplate.Font do
  begin
    CharSet:= GetCharSet;
    //Byte(PtrInt(TObject(cbbFontCharset.Items[cbbFontCharset.ItemIndex])));
    Color:= clboxFontColor.Selected;
    Name:= lbxFontFamily.Items[lbxFontFamily.ItemIndex];
    Pitch:= TFontPitch(GetPitch);

    if (Length(edtFontSize.Text) > 0)
      then Size:= StrToInt(edtFontSize.Text)
      else Size:= StrToInt(lbxFontSize.Items[lbxFontSize.ItemIndex]);

    Style:= [];
    i:= PtrInt(lbxStyles.Items.Objects[lbxStyles.ItemIndex]);
    if ((i and 1) <> 0) then Style := Style + [fsItalic];
    if ((i and 2) <> 0) then Style := Style + [fsBold];

    if chbFontStyleUnderline.Checked
      then Style := Style + [fsUnderline]
      else Style := Style - [fsUnderline];
    if chbFontStyleStrike.Checked
      then Style := Style + [fsStrikeOut]
      else Style := Style - [fsStrikeOut];
  end;
end;

end.

