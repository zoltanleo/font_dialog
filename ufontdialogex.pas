unit ufontdialogex;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , ColorBox
  , Controls
  , ExtCtrls
  , SysUtils
  , Forms
  , Graphics
  , Dialogs
  , StdCtrls
  , Grids
  , LCLType
  , LCLIntf
  , LazUTF8
  , Types
  ;

type

  { TfrmFontDialogEx }

  TfrmFontDialogEx = class(TForm)
    btnApplyFilter: TButton;
    btnLeft: TButton;
    btnRight: TButton;
    cbbFontCharset: TComboBox;
    cbbFontPitch: TComboBox;
    chbStrike: TCheckBox;
    chbUnderLine: TCheckBox;
    clboxFontColor: TColorBox;
    edtFontSize: TEdit;
    edtFontFamily: TEdit;
    gbEffects: TGroupBox;
    gbFilter: TGroupBox;
    gdSample: TStringGrid;
    lblFontCharset: TLabel;
    lblFontColor: TLabel;
    lblFontFamily: TLabel;
    lblSample: TLabel;
    lblFontSizes: TLabel;
    lblFontStyles: TLabel;
    lbxFontCharset: TListBox;
    lbxFontFamily: TListBox;
    lbxFontSizes: TListBox;
    lbxFontStyles: TListBox;
    pnlComp: TPanel;
    pnlBtn: TPanel;
    scrboxComp: TScrollBox;
    splFamilyFontHorz: TSplitter;
    splFamilyFontVert: TSplitter;
    splgbEffects: TSplitter;
    procedure btnResetTextClick(Sender: TObject);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure chbStrikeChange(Sender: TObject);
    procedure chbUnderLineChange(Sender: TObject);
    procedure clboxFontColorChange(Sender: TObject);
    procedure edtFontFamilyEditingDone(Sender: TObject);
    procedure edtFontSizeEditingDone(Sender: TObject);
    procedure edtFontSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxFontFamilyClick(Sender: TObject);
    procedure lbxFontCharsetClick(Sender: TObject);
    procedure lbxFontSizesClick(Sender: TObject);
    procedure lbxFontStylesClick(Sender: TObject);
  private
    FFontSizeValueMax: PtrInt;
    FFontSizeValueMin: PtrInt;
    FSelfFont: TFont;
    FTime: LongWord;
    FIniTime: LongWord;
    FCharSize: TSize;
    FCurrentFontSize: PtrInt;
    procedure SetSelfFont(AValue: TFont);
    procedure StartTimer;
    Procedure EndTimer;
    function  GetCharSet: byte;
    function  GetPitch: integer;
    procedure EnableEvents(Ok: boolean; Lb: TListbox = nil);
    procedure SelectFont;
    procedure ResetSampleText;
    procedure LoadFontList;
    procedure LoadFamilyFonts(Charset: integer);
    procedure UpdateFont(F: TFont);
    procedure FillcbbCharSet;
  public
    property SelfFont: TFont read FSelfFont write SetSelfFont;
    property FontSizeValueMin: PtrInt read FFontSizeValueMin write FFontSizeValueMin;
    property FontSizeValueMax: PtrInt read FFontSizeValueMax write FFontSizeValueMax;

    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  end; 

const
  cFontSizeValueMin = 4;
  cFontSizeValueMax = 100;
var
  frmFontDialogEx: TfrmFontDialogEx;

implementation
{.$define Debug}

{$R *.lfm}
{$I 'ufontdlgex_i18n.inc'}

{ TfrmFontDialogEx }
var
  LStyles,
  LSizes: TStringList;

function EnumFontsNoDups(
  var LogFont: TEnumLogFontEx;
  var Metric: TNewTextMetricEx;
  FontType: Longint;
  Data: LParam):LongInt; stdcall;
var
  L: TStringList;
  S: String;
begin
  L := TStringList(ptrint(Data));
  S := LogFont.elfLogFont.lfFaceName;
  if L.IndexOf(S)<0 then
    L.Add(S);
  result := 1;
end;

var
  NeedTTF: boolean;
  
function EnumFamilyFonts(
  var eLogFont: TEnumLogFontEx;
  var Metric:TNewTextMetricEx;
  FontType:longint;
  Data:LParam):longint; stdcall;
var
  s: string;
  n: integer;
  lcharsets: TStringList;
begin
  LCharSets := TStringList(ptrint(Data));
  if (Lcharsets <> nil) then begin
    // collect charsets
    // when collecting charsets no need to collect all other info
    s:= CharSetToString(eLogFont.elfLogFont.lfCharSet);
    if (LCharsets.indexOf(s) < 0) then
      LCharsets.AddObject(s, TObject(ptrint(eLogFont.elfLogFont.lfCharSet)));
    exit;
  end;
  
  // collect styles
  s :=eLogFont.elfStyle;
  if (LStyles.IndexOf(s) < 0) then begin
    // encode bold (bit 0), italic (bit 1) -- see SelectFont
    n := 0;
    {$IF DEFINED(LCLWin32) or DEFINED(LCLCocoa) }
    if (eLogFont.elfLogFont.lfItalic <> 0) then
      n := n or 1;
    if (eLogFont.elfLogFont.lfWeight > FW_MEDIUM) then
      n := n or 2;
    {$ENDIF}
    {$IF DEFINED(LCLGtk2) or DEFINED(LCLGtk3) or DEFINED(LCLQt) or DEFINED(LCLQt5)}
    s := Lowercase(s);
    if (pos('italic', s) <> 0) or (pos('oblique', s) <> 0) then
      n := n or 1;
    if (pos('bold', s) <> 0) then
      n := n or 2;
    {$ENDIF}
    LStyles.AddObject(eLogFont.elfStyle, TObject(PtrInt(n)));
  end;
  
  // collect sizes
  if (FontType=TRUETYPE_FONTTYPE)
  then
    NeedTTF := True
  else
    with metric.ntmentm do
      if (tmDigitizedAspectY <> 0) then
      begin
        n:= (tmHeight-tmInternalLeading)*72 + tmDigitizedAspectY shr 1;
        n:= n div tmDigitizedAspectY;
        if (n > 0) then
        begin
          s := IntToStr(n)+'*'; // font sizes with * indicate raster fonts
          if LSizes.IndexOf(s)<0 then
            LSizes.AddObject(s, TObject(ptrint(n)));
        end;
      end;
  result := 1;
end;

procedure TfrmFontDialogEx.btnResetTextClick(Sender: TObject);
begin
  ResetSampleText;
end;

procedure TfrmFontDialogEx.btnApplyFilterClick(Sender: TObject);
var
  i: PtrInt = -1;
begin
  LoadFontList;
  i:= cbbFontCharset.ItemIndex;
  LoadFamilyFonts(byte(i));
  SelectFont;
end;

procedure TfrmFontDialogEx.chbStrikeChange(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmFontDialogEx.chbUnderLineChange(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmFontDialogEx.clboxFontColorChange(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmFontDialogEx.edtFontFamilyEditingDone(Sender: TObject);
var
  n: PtrInt = 0;
  edtText: String = '';
begin
  if (lbxFontFamily.Count = 0) then Exit;

  edtText:= UTF8LowerCase(UTF8Trim(edtFontFamily.Text));

  if (edtText <> '') then
  begin
    n:= lbxFontFamily.Items.IndexOf(edtText);
    if (n <> -1)
      then lbxFontFamily.ItemIndex:= n
      else
        for n:= 0 to Pred(lbxFontFamily.Count) do
          begin
            if ( UTF8Pos(edtText, UTF8LowerCase(lbxFontFamily.Items[n])) > 0) then
            begin
              lbxFontFamily.ItemIndex:= n;
              Break;
            end;
          end;
  end;
end;

procedure TfrmFontDialogEx.edtFontSizeEditingDone(Sender: TObject);
var
  i: PtrInt = -1;
  tmpStr: String = '';
begin
  if (StrToInt(edtFontSize.Text) < FontSizeValueMin)
    then edtFontSize.Text:= IntToStr(FontSizeValueMin);

  if (StrToInt(edtFontSize.Text) > FontSizeValueMax)
    then edtFontSize.Text:= IntToStr(FontSizeValueMax);

  FCurrentFontSize:= StrToInt(edtFontSize.Text);

  {$IFNDEF DARWIN}
  lbxFontSizes.ItemIndex:= -1;
  {$ENDIF}

  i:= lbxFontSizes.Items.IndexOf(edtFontSize.Text);
  if (i <> -1)
    then
      lbxFontSizes.ItemIndex:= i
    else
      for i:= 0 to Pred(lbxFontSizes.Count) do
      begin
        tmpStr:= lbxFontSizes.Items[i];
        if (pos('*',tmpStr) > 0) then tmpStr:= Copy(tmpStr, 1, pos('*',tmpStr) - 1);

        if (edtFontSize.Text = tmpStr) then
        begin
          lbxFontSizes.ItemIndex:= i;
          Break;
        end;
      end;

  SelectFont;
end;

procedure TfrmFontDialogEx.edtFontSizeKeyPress(Sender: TObject; var Key: char);
begin
  //only numbers and backspace key are allowed to be pressed
  if ((Ord(Key) <> VK_BACK) and ((ord(Key) < VK_0) or (ord(Key) > VK_9))) then Key:= #0;
end;

procedure TfrmFontDialogEx.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmFontDialogEx.FormCreate(Sender: TObject);
var
  i: PtrInt = 0;
  txtlen: PtrInt = 0;
begin
  FCharSize.cx:= Self.Canvas.TextWidth('W');
  FCharSize.cy:= Self.Canvas.TextHeight('Wj');

  FillcbbCharSet;
  ResetSampleText;

  FSelfFont:= TFont.Create;
  FSelfFont.Assign(Screen.SystemFont);
  FCurrentFontSize:= Screen.SystemFont.Size;
  FFontSizeValueMin:= cFontSizeValueMin;
  FFontSizeValueMax:= cFontSizeValueMax;
  edtFontSize.MaxLength:= 3;

  with Self do
  begin
    //{darwin w: 13 | h: 15}
    //{ms windows w: 11 | h: 15}
    //{linux w: 15 | h: 19}
    //Caption:= Format('w: %d | h: %d',[sz.cx, sz.cy]);
    Caption:= cTitle;
    ModalResult:= mrCancel;
    Position:= poScreenCenter;
  end;

  {$IF DEFINED(MSWINDOWS) or DEFINED(LCLQt) or DEFINED(LCLQt5)}
  btnLeft.Caption:= cBtnOKCaption;
  btnRight.Caption:= cBtnCancelCaption;
  btnLeft.OnClick:= @BtnOKClick;
  btnRight.OnClick:= @BtnCancelClick;
  {$ELSE}
  btnLeft.Caption:= cBtnCancelCaption;
  btnRight.Caption:= cBtnOKCaption;
  btnLeft.OnClick:= @BtnCancelClick;
  btnRight.OnClick:= @BtnOKClick;
  {$ENDIF}

  for i:= 0 to Pred(pnlBtn.ControlCount) do
    if TObject(pnlBtn.Controls[i]).InheritsFrom(TButton) then
      if (txtlen < pnlBtn.Canvas.TextWidth(TButton(pnlBtn.Controls[i]).Caption)) then
        txtlen:= pnlBtn.Canvas.TextWidth(TButton(pnlBtn.Controls[i]).Caption);

  for i:= 0 to Pred(pnlBtn.ControlCount) do
    if TObject(pnlBtn.Controls[i]).InheritsFrom(TButton) then
    begin
      {$IFDEF MSWINDOWS}
      TButton(pnlBtn.Controls[i]).AutoSize:= True;
      {$ELSE}
        {$IFDEF LINUX}
        TButton(pnlBtn.Controls[i]).AutoSize:= False;
        TButton(pnlBtn.Controls[i]).Height:= FCharSize.cx * 2;
        {$ENDIF}
      {$ENDIF}

      TButton(pnlBtn.Controls[i]).Constraints.MinWidth:= txtlen + FCharSize.cx * 2;
      TButton(pnlBtn.Controls[i]).ShowHint:= True;
    end;

end;

procedure TfrmFontDialogEx.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelfFont);
end;

procedure TfrmFontDialogEx.FormShow(Sender: TObject);
var
  i: PtrInt = -1;
begin
  lblFontStyles.Caption:= cFontStyles;
  lblFontSizes.Caption:= cFontSize;
  gbEffects.Caption:= cFontEffects;
  chbStrike.Caption:= cStrikeout;
  chbUnderLine.Caption:= cUnderline;
  gbFilter.Caption:= cFontFilter;
  lblFontColor.Caption:= cFontColor;
  lblSample.Caption:= cFontSample;
  lblFontCharset.Caption:= cFontCharSet;
  btnApplyFilter.Caption:= cBtnApplyFilter;

  if (SelfFont.Size <> 0)
    then FCurrentFontSize:= SelfFont.Size
    else FCurrentFontSize:= Screen.SystemFont.Size;

  edtFontSize.Text:= IntToStr(FCurrentFontSize);
  clboxFontColor.Selected:= SelfFont.Color;
  chbStrike.Checked:= (fsStrikeOut in SelfFont.Style);
  chbUnderLine.Checked:= (fsUnderline in SelfFont.Style);

  LoadFontlist;

  if (lbxFontFamily.ItemIndex < 0)
    then edtFontFamily.Clear
    else edtFontFamily.Text:= lbxFontFamily.Items[lbxFontFamily.ItemIndex];

  lbxFontCharsetClick(nil);
  if (lbxFontCharset.Count > 0) then
  begin
    i:= cbbFontCharset.Items.IndexOf(lbxFontCharset.Items[lbxFontCharset.ItemIndex]);
    if (i <> -1)
      then cbbFontCharset.ItemIndex:= i
      else cbbFontCharset.ItemIndex:= 1;
  end;

  lbxFontStylesClick(Self);
  edtFontSizeEditingDone(Self);

  lbxFontSizes.Constraints.MinWidth:=  lblFontSizes.Width + FCharSize.cx * 2;
  lbxFontStyles.Constraints.MinWidth:= lblFontStyles.Width + FCharSize.cx * 2;
  gbEffects.Constraints.MinWidth:= chbStrike.Width * 2 + FCharSize.cx * 2;
  gdSample.Constraints.MinWidth:= gbEffects.Constraints.MinWidth;
  lbxFontFamily.Constraints.MinWidth:= lblFontFamily.Width + FCharSize.cx * 2;
  lbxFontFamily.Constraints.MinHeight:= FCharSize.cx * 4;
  lbxFontFamily.Constraints.MaxHeight:= FCharSize.cx * 14;

  gbFilter.Constraints.MinHeight:= cbbFontCharset.Top +
                                   cbbFontPitch.BorderSpacing.Top +
                                   cbbFontCharset.Height * 2 +
                                   btnApplyFilter.BorderSpacing.Top +
                                   btnApplyFilter.Height +
                                   btnApplyFilter.BorderSpacing.Bottom +
                                   FCharSize.cx * 2;

  splFamilyFontVert.Left:= lbxFontFamily.Left +
                           lbxFontFamily.Width +
                           lbxFontFamily.BorderSpacing.Right +
                           FCharSize.cx * 2;

  splFamilyFontHorz.Top:= lbxFontFamily.Top +
                          lbxFontFamily.Height +
                          lbxFontFamily.BorderSpacing.Bottom +
                          FCharSize.cx * 2;

  splgbEffects.Left:= gbEffects.Left +
                      gbEffects.Width +
                      gbEffects.BorderSpacing.Right +
                      FCharSize.cx * 2;

  with pnlComp do
  begin
    AutoSize:= True;
    ParentColor:= True;
    BevelOuter:= bvNone;
    Caption:= '';
  end;

  with pnlBtn do
  begin
    AutoSize:= True;
    ParentColor:= True;
    BevelOuter:= bvNone;
    Caption:= '';
  end;

  with scrboxComp do
  begin
    BorderStyle:= bsNone;
    HorzScrollBar.Smooth:= True;
    HorzScrollBar.Tracking:= True;
    VertScrollBar.Smooth:= True;
    VertScrollBar.Tracking:= True;
  end;

  Self.Constraints.MinWidth:= FCharSize.cx * 60;
  {$IFDEF LINUX}
  Self.Constraints.MinHeight:= FCharSize.cy * 18;
  {$ELSE}
  Self.Constraints.MinHeight:= FCharSize.cy * 20;
  {$ENDIF}

  with Self do
  begin
    BorderIcons:= [biSystemMenu];
    AutoSize:= True;
    BorderStyle:= bsSizeable;
  end;
end;

procedure TfrmFontDialogEx.lbxFontFamilyClick(Sender: TObject);
var
  i: PtrInt = -1;
begin
  if (lbxFontFamily.ItemIndex < 0)
    then edtFontFamily.Clear
    else edtFontFamily.Text:= lbxFontFamily.Items[lbxFontFamily.ItemIndex];

  LoadFamilyFonts(-1);
  lbxFontCharsetClick(nil);

  if (lbxFontCharset.Count > 0) then
  begin
    i:= cbbFontCharset.Items.IndexOf(lbxFontCharset.Items[lbxFontCharset.ItemIndex]);
    if (i <> -1)
      then cbbFontCharset.ItemIndex:= i
      else cbbFontCharset.ItemIndex:= 1;
  end;

  SelectFont;
end;

procedure TfrmFontDialogEx.lbxFontCharsetClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbxFontCharset.ItemIndex;
  if (i < 0) then exit;
  i := PtrInt(lbxFontCharset.Items.Objects[i]);
  LoadFamilyFonts(byte(i));
end;

procedure TfrmFontDialogEx.lbxFontSizesClick(Sender: TObject);
var
  tmpStr: String = '';
begin
  tmpStr:= lbxFontSizes.Items[lbxFontSizes.ItemIndex];
  if (pos('*',tmpStr) > 0) then tmpStr:= Copy(tmpStr, 1, pos('*',tmpStr) - 1);

  if (edtFontSize.Text <> tmpStr) then edtFontSize.Text:= tmpStr;

  FCurrentFontSize:= StrToInt(tmpStr);

  SelectFont;
end;

procedure TfrmFontDialogEx.lbxFontStylesClick(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmFontDialogEx.StartTimer;
begin
  FIniTime := GetTickCount;
end;

procedure TfrmFontDialogEx.SetSelfFont(AValue: TFont);
begin
  if FSelfFont.Equals(AValue) then Exit;
  FSelfFont.Assign(AValue);
end;

procedure TfrmFontDialogEx.EndTimer;
begin
  FTime := GetTickCount-FIniTime;
end;

function TfrmFontDialogEx.GetCharSet: byte;
begin
  if cbbFontCharset.Itemindex<0 then
    result := ANSI_CHARSET
  else
    result := Byte(PtrInt(cbbFontCharset.items.Objects[cbbFontCharset.ItemIndex]));
end;

function TfrmFontDialogEx.GetPitch: integer;
begin
  case cbbFontPitch.ItemIndex of
    1: result := FIXED_PITCH;
    2: result := VARIABLE_PITCH;
    3: result := MONO_FONT;
    else
      result := DEFAULT_PITCH;
  end;
end;

procedure TfrmFontDialogEx.EnableEvents(Ok: boolean; Lb: TListbox = nil);
  procedure SetEvent(L: TListbox);
  var
    Event: TNotifyEvent;
  begin
    Event := nil;
    if ok then begin
      if l=lbxFontFamily then Event := @lbxFontFamilyClick else
      if l=lbxFontStyles then Event := @lbxFontStylesClick else
      if l=lbxFontCharset then Event := @lbxFontCharsetClick else
      if l=lbxFontSizes then Event := @lbxFontSizesClick;
    end;
    L.OnClick := Event;
  end;
begin
  if Lb<>nil then
    SetEvent(Lb)
  else begin
    SetEvent(lbxFontFamily);
    SetEvent(lbxFontStyles);
    SetEvent(lbxFontCharset);
    SetEvent(lbxFontSizes);
  end;
end;

procedure TfrmFontDialogEx.SelectFont;
var
  i: integer;
begin
  if (lbxFontFamily.ItemIndex >= 0) then
    if (lbxFontCharset.ItemIndex >= 0) then
      if (lbxFontStyles.ItemIndex >= 0) then
      begin
        FSelfFont.Name := lbxFontFamily.Items[lbxFontFamily.ItemIndex];
        FSelfFont.CharSet := TFontCharSet(ptrint(lbxFontCharset.Items.Objects[lbxFontCharset.ItemIndex]));
        FSelfFont.Size:= FCurrentFontSize;
        i:= PtrInt(lbxFontStyles.Items.Objects[lbxFontStyles.ItemIndex]);
        FSelfFont.Style:= [];
        if ((i and 1) <> 0) then FSelfFont.Style:= FSelfFont.Style + [fsItalic];
        if ((i and 2) <> 0) then FSelfFont.Style:= FSelfFont.Style + [fsBold];
        if chbUnderLine.Checked then FSelfFont.Style:= FSelfFont.Style + [fsUnderline];
        if chbStrike.Checked then FSelfFont.Style:= FSelfFont.Style + [fsStrikeOut];
        FSelfFont.Color:= clboxFontColor.Selected;
        UpdateFont(FSelfFont);
      end;
end;

procedure TfrmFontDialogEx.ResetSampleText;
var
  SL: TStringList = nil;
begin
  SL:= TStringList.Create;
  try
    SL.Add('abcdefghijklmnopqrstuvwxyz');
    SL.Add('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
    SL.Add('01234567891   ўЈ¤Ґ§');
    SL.Add('абвгдежзийклмнопрстуфхцшщъыь');
    SL.add('АБВГДЕЖЗИЙКЛМНОПРСТУФХХШЩЪЫЬЭЯ');
    gdSample.Cols[0] := SL;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmFontDialogEx.LoadFontList;
var
  DC: HDC;
  lf: TLogFont;
  L: TStringList;
  i: Integer;
begin
  // this could be have done also with screen.fonts
  // but here, we have the list filtered by Charset
  lf.lfCharSet:= GetCharSet;
  lf.lfFaceName:= '';
  case cbbFontPitch.ItemIndex of
    1: i:=FIXED_PITCH;
    2: i:=VARIABLE_PITCH;
    3: i:=MONO_FONT;
    else
      i:= DEFAULT_PITCH;
  end;

  lf.lfPitchAndFamily:= i;
  L:= TStringList.create;
  lbxFontStyles.Clear;
  lbxFontCharset.Clear;
  lbxFontSizes.Clear;

  DC := GetDC(0);
  EnableEvents(False, lbxFontFamily);
  try
    EnumFontFamiliesEX(DC, @lf, @EnumFontsNoDups, ptrint(L), 0);
    L.Sort;
    lbxFontFamily.Items.Assign(L);
    lbxFontFamily.Itemindex:= -1;
    
    if (LowerCase(SelfFont.Name) = 'default')
      then lbxFontFamily.ItemIndex:= lbxFontFamily.Items.IndexOf(Screen.SystemFont.Name)
      else lbxFontFamily.ItemIndex:= lbxFontFamily.Items.IndexOf(SelfFont.Name);

    if (lbxFontFamily.ItemIndex < 0) then
    begin
      if (lbxFontFamily.Items.Count > 0) then
        lbxFontFamily.ItemIndex:= 0;
    end;
    LoadFamilyFonts(-1);
    lblFontFamily.Caption := format(cFontFaceList,[lbxFontFamily.Items.Count]);
  finally
    EnableEvents(True, lbxFontFamily);
    ReleaseDC(0, DC);
    L.Free;
  end;
end;

function CompareSizes(List: TStringList; Index1, Index2: Integer): Integer;
begin
  result := ptrint(List.Objects[Index1]) - ptrint(List.Objects[Index2]);
end;

procedure TfrmFontDialogEx.LoadFamilyFonts(Charset: integer);
var
  LCharset: TStringList;
  dc: HDC;
  Lf: TLogFont;
  i: LongInt;
  LoadingCharsets: boolean;
  n: PtrInt = 0;
  
  procedure AddScalableSizes;
    procedure Add(Sz: Integer);
    begin
      if LSizes.IndexOfObject(TObject(ptrint(Sz)))<0 then
        LSizes.AddObject(IntToStr(Sz), TObject(ptrint(Sz)));
    end;
  begin
    add(8);  add(9);  add(10); add(11); add(12); add(14); add(16); add(18);
    add(20); add(22); add(24); add(26); add(28); add(36); add(48); add(72);
  end;
begin
  i:= lbxFontFamily.ItemIndex;
  if (i < 0) then exit;
  
  LoadingCharsets:= (Charset < 0);

  {$ifdef debug}
  Write('LoadFamilyFonts: for family=', lbxFontFamily.Items[i],' and Charset=');
  if LoadingCharsets then
    debugln('ALL_CHARSETS')
  else
    debugln(CharsetToString(byte(Charset)));
  {$endif}

  // at the moment only global fonts are enumerated
  // ie. fonts selected in a device context are not enumerated
  DC := GetDC(0);
  // create global variables, EnumFamilyFonts use them
  if LoadingCharsets then begin
    // need to fill charset listbox too
    LCharset := TStringList.Create;
    CharSet := DEFAULT_CHARSET;
  end else begin
    // charset listbox is already filled, so fill styles and sizes
    LCharSet := nil;
    LStyles := TStringList.Create;
    LSizes  := TStringList.Create;
  end;
  try
    // enumerate fonts
    Lf.lfFaceName:= lbxFontFamily.Items[i];
    Lf.lfCharSet:= byte(Charset);
    Lf.lfPitchAndFamily := 0;
    NeedTTF:= False;
    EnumFontFamiliesEX(DC, @Lf, @EnumFamilyFonts, ptrint(LCharset), 0);
    // fill charset listbox if necessary
    if (LCharset <> nil)
    then begin
          LCharset.Sort;
          EnableEvents(False, lbxFontCharset);
          lbxFontCharset.Items.Assign(LCharset);
          lbxFontCharset.ItemIndex := -1;
          EnableEvents(true, lbxFontCharset);
         end
    else begin
          // fill styles listbox
          LStyles.Sort;
          EnableEvents(False, lbxFontStyles);
          lbxFontStyles.Items.Assign(LStyles);
          lbxFontStyles.ItemIndex := -1;
          EnableEvents(true, lbxFontStyles);

          //load and set font styles
          n:= 0;//regular = non fsBold and non fsItalic
          if (fsItalic in SelfFont.Style) then n:= (n or 1); //fsItalic and regular
          if (fsBold in SelfFont.Style) then n:= (n or 2);//fsbold and regular/italic

          lbxFontStyles.ItemIndex:= lbxFontStyles.Items.IndexOfObject(TObject(PtrInt(n)));

          if (lbxFontStyles.ItemIndex < 0) then
            if (lbxFontStyles.Items.Count > 0) then lbxFontStyles.ItemIndex:= 0;


          // fill sizes listbox
          // any raster font size is already there
          if NeedTTF then AddScalableSizes;
          LSizes.CustomSort(@CompareSizes);
          EnableEvents(False, lbxFontSizes);
          lbxFontSizes.Items.Assign(LSizes);
          lbxFontSizes.ItemIndex := -1;
          EnableEvents(true, lbxFontSizes);

          lbxFontSizes.ItemIndex:= lbxFontSizes.Items.IndexOf(IntToStr(FCurrentFontSize));

          if (lbxFontSizes.ItemIndex < 0) then
            if (lbxFontSizes.Items.Count > 0) then lbxFontSizes.ItemIndex := 0;
         end;
  finally
    if (LCharset = nil)
    then begin
          LSizes.Free;
          LStyles.Free;
         end
    else
      LCharset.Free;

    releaseDC(0, DC);
  end;
  
  if LoadingCharsets then begin
    // make an initial charset selection
    lbxFontCharset.ItemIndex:= lbxFontCharset.Items.IndexOf(CharSetToString(PtrInt(SelfFont.CharSet)));

    if (lbxFontCharset.ItemIndex < 0) then begin
      if (lbxFontCharset.Items.Count > 0) then
        lbxFontCharset.ItemIndex := 0;
    end;
  end;
end;

procedure TfrmFontDialogEx.UpdateFont(F: TFont);
begin
  gdSample.Font := F;
  gdSample.DefaultRowHeight := gdSample.canvas.textHeight('Wj') + 5;
end;

procedure TfrmFontDialogEx.FillcbbCharSet;
  procedure Add(Charset: Integer);
  begin
    cbbFontCharset.Items.AddObject(CharSetToString(CharSet), TObject(ptrint(Charset)));
  end;
begin
  // populate cbbFontCharset
  cbbFontCharset.Items.clear;
  Add(ANSI_CHARSET);
  Add(DEFAULT_CHARSET);
  Add(SYMBOL_CHARSET);
  Add(MAC_CHARSET);
  Add(SHIFTJIS_CHARSET);
  Add(HANGEUL_CHARSET);
  Add(JOHAB_CHARSET);
  Add(GB2312_CHARSET);
  Add(CHINESEBIG5_CHARSET);
  Add(GREEK_CHARSET);
  Add(TURKISH_CHARSET);
  Add(VIETNAMESE_CHARSET);
  Add(HEBREW_CHARSET);
  Add(ARABIC_CHARSET);
  Add(BALTIC_CHARSET);
  Add(RUSSIAN_CHARSET);
  Add(THAI_CHARSET);
  Add(EASTEUROPE_CHARSET);
  Add(OEM_CHARSET);
  Add(FCS_ISO_10646_1);
  Add(FCS_ISO_8859_1);
  Add(FCS_ISO_8859_2);
  Add(FCS_ISO_8859_3);
  Add(FCS_ISO_8859_4);
  Add(FCS_ISO_8859_5);
  Add(FCS_ISO_8859_6);
  Add(FCS_ISO_8859_7);
  Add(FCS_ISO_8859_8);
  Add(FCS_ISO_8859_9);
  Add(FCS_ISO_8859_10);
  Add(FCS_ISO_8859_15);

  cbbFontCharset.ItemIndex:= 1;
end;

procedure TfrmFontDialogEx.BtnOKClick(Sender: TObject);
begin
  Self.ModalResult:= mrOK;
end;

procedure TfrmFontDialogEx.BtnCancelClick(Sender: TObject);
begin
  Self.ModalResult:= mrCancel;
end;

end.

