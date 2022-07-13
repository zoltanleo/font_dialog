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
    cbbCharset: TComboBox;
    cbbPitch: TComboBox;
    chbStrike: TCheckBox;
    chbUnderLine: TCheckBox;
    clboxFontColor: TColorBox;
    Edit1: TEdit;
    edtFamily: TEdit;
    gbEffects: TGroupBox;
    gbFilter: TGroupBox;
    grid: TStringGrid;
    lblCharset: TLabel;
    lblFontColor: TLabel;
    lblFontFaceList: TLabel;
    lblSample: TLabel;
    lblSizes: TLabel;
    lblStyles: TLabel;
    lbxCharset: TListBox;
    lbxFamily: TListBox;
    lbxSizes: TListBox;
    lbxStyles: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    splFamilyFontHorz: TSplitter;
    splFamilyFontVert: TSplitter;
    splgbEffects: TSplitter;
    procedure btnResetTextClick(Sender: TObject);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure chbStrikeChange(Sender: TObject);
    procedure chbUnderLineChange(Sender: TObject);
    procedure clboxFontColorChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxFamilyClick(Sender: TObject);
    procedure lbxCharsetClick(Sender: TObject);
    procedure lbxSizesClick(Sender: TObject);
    procedure lbxStylesClick(Sender: TObject);
  private
    FSelfFont: TFont;
    FTime: LongWord;
    FIniTime: LongWord;
    FCurrentFamily
    ,FCurrentStyle
    ,FCurrentSize
    ,FCurrentCharset
    , FCurrentColor: string;
    FCharSize: TSize;
    procedure SetSelfFont(AValue: TFont);
    procedure StartTimer;
    Procedure EndTimer;
    function  GetCharSet: byte;
    function  GetPitch: integer;
    procedure EnableEvents(Ok: boolean; Lb: TListbox = nil);
    procedure SelectFont;
    procedure ResetSampleText;
    procedure SaveSelection;
    procedure RestoreSelection(Sender: TListbox);
    procedure LoadFontList;
    procedure LoadFamilyFonts(Charset: integer);
    procedure UpdateFont(F: TFont);
    procedure FillcbbCharSet;
  public
    property SelfFont: TFont read FSelfFont write SetSelfFont;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  end; 

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
  if FontType=TRUETYPE_FONTTYPE then
    NeedTTF := True
  else
    with metric.ntmentm do
      if tmDigitizedAspectY <> 0 then begin
        n := (tmHeight-tmInternalLeading)*72+tmDigitizedAspectY shr 1;
        n := n div tmDigitizedAspectY;
        if n>0 then begin
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
  i:= cbbCharset.ItemIndex;
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

  {$IFDEF MSWINDOWS}
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

  //{$IFDEF MSWINDOWS}
  //if (UTF8LowerCase(ShortCutToText(ActBtnCancel.ShortCut)) <> 'unknown')
  //  then btnRight.Hint:= Format('<%s>',[ShortCutToText(ActBtnCancel.ShortCut)])
  //  else btnRight.Hint:= '';
  //if (UTF8LowerCase(ShortCutToText(ActBtnOK.ShortCut)) <> 'unknown')
  //  then btnLeft.Hint:= Format('<%s>',[ShortCutToText(ActBtnOK.ShortCut)])
  //  else btnRight.Hint:= '';
  //
  //{$ELSE}
  //if (UTF8LowerCase(ShortCutToText(ActBtnCancel.ShortCut)) <> 'unknown')
  //  then btnLeft.Hint:= Format('<%s>',[ShortCutToText(ActBtnCancel.ShortCut)])
  //  else btnLeft.Hint:= '';
  //if (UTF8LowerCase(ShortCutToText(ActBtnOK.ShortCut)) <> 'unknown')
  //  then btnRight.Hint:= Format('<%s>',[ShortCutToText(ActBtnOK.ShortCut)])
  //  else btnRight.Hint:= '';
  //{$ENDIF}

  for i:= 0 to Pred(Self.ControlCount) do
    if TObject(Self.Controls[i]).InheritsFrom(TButton) then
      if (txtlen < Self.Canvas.TextWidth(TButton(Self.Controls[i]).Caption)) then
        txtlen:= Self.Canvas.TextWidth(TButton(Self.Controls[i]).Caption);

  for i:= 0 to Pred(Self.ControlCount) do
    if TObject(Self.Controls[i]).InheritsFrom(TButton) then
    begin
      {$IFDEF MSWINDOWS}
      TButton(Self.Controls[i]).AutoSize:= True;
      {$ELSE}
        {$IFDEF LINUX}
        TButton(Self.Controls[i]).AutoSize:= False;
        TButton(Self.Controls[i]).Height:= FCharSize.cx * 3 div 2;
        {$ENDIF}
      {$ENDIF}

      TButton(Self.Controls[i]).Constraints.MinWidth:= txtlen + FCharSize.cx * 2;
      TButton(Self.Controls[i]).ShowHint:= True;
    end;

end;

procedure TfrmFontDialogEx.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelfFont);
end;

procedure TfrmFontDialogEx.FormShow(Sender: TObject);
var
  i: PtrInt = -1;
  n: PtrInt = 0;
begin
  lblStyles.Caption:= cFontStyles;
  lblSizes.Caption:= cFontSize;
  gbEffects.Caption:= cFontEffects;
  chbStrike.Caption:= cStrikeout;
  chbUnderLine.Caption:= cUnderline;
  gbFilter.Caption:= cFontFilter;
  lblFontColor.Caption:= cFontColor;
  lblSample.Caption:= cFontSample;
  lblCharset.Caption:= cFontCharSet;
  btnApplyFilter.Caption:= cBtnApplyFilter;

  if (LowerCase(SelfFont.Name) = 'default')
    then FCurrentFamily:= Screen.SystemFont.Name
    else FCurrentFamily:= SelfFont.Name;

  FCurrentCharset:= CharSetToString(SelfFont.CharSet);
  FCurrentSize:= IntToStr(SelfFont.Size);
  clboxFontColor.Selected:= SelfFont.Color;
  chbStrike.Checked:= (fsStrikeOut in SelfFont.Style);
  chbUnderLine.Checked:= (fsUnderline in SelfFont.Style);

  LoadFontlist;
  lbxCharsetClick(nil);
  if (lbxCharset.Count > 0) then
  begin
    i:= cbbCharset.Items.IndexOf(lbxCharset.Items[lbxCharset.ItemIndex]);
    if (i <> -1)
      then cbbCharset.ItemIndex:= i
      else cbbCharset.ItemIndex:= 1;
  end;

  //load and set font styles
  n:= 0;//regular = non fsBold and non fsItalic

  if (fsItalic in SelfFont.Style) then n:= (n or 1); //fsItalic and regular
  if (fsBold in SelfFont.Style) then n:= (n or 2);//fsbold and regular/italic

  i:= lbxStyles.Items.IndexOfObject(TObject(PtrInt(n)));

  if (i <> -1)
    then lbxStyles.ItemIndex:= i
    else
      if (lbxStyles.Count > 0)
        then lbxStyles.ItemIndex:= 0
        else lbxStyles.ItemIndex:= -1;

  lbxStylesClick(Self);

  lbxSizes.Constraints.MinWidth:=  lblSizes.Width + FCharSize.cx * 2;
  lbxStyles.Constraints.MinWidth:= lblStyles.Width + FCharSize.cx * 2;
  gbEffects.Constraints.MinWidth:= chbStrike.Width * 2 + FCharSize.cx * 2;
  grid.Constraints.MinWidth:= gbEffects.Constraints.MinWidth;
  lbxFamily.Constraints.MinWidth:= lblFontFaceList.Width + FCharSize.cx * 2;
  lbxFamily.Constraints.MinHeight:= FCharSize.cx * 4;
  lbxFamily.Constraints.MaxHeight:= FCharSize.cx * 14;

  gbFilter.Constraints.MinHeight:= cbbCharset.Top +
                                   cbbPitch.BorderSpacing.Top +
                                   cbbCharset.Height * 2 +
                                   btnApplyFilter.BorderSpacing.Top +
                                   btnApplyFilter.Height +
                                   btnApplyFilter.BorderSpacing.Bottom +
                                   FCharSize.cx * 2;

  //splFamilyFontVert.Left:= lbxFamily.Left +
  //                         lbxFamily.Width +
  //                         lbxFamily.BorderSpacing.Right +
  //                         FCharSize.cx * 2;

  splFamilyFontHorz.Top:= lbxFamily.Top +
                          lbxFamily.Height +
                          lbxFamily.BorderSpacing.Bottom +
                          FCharSize.cx * 2;
  splgbEffects.Left:= gbEffects.Left +
                      gbEffects.Width +
                      gbEffects.BorderSpacing.Right +
                      FCharSize.cx * 2;

  Self.Constraints.MinWidth:= FCharSize.cx * 60;
  {$IFDEF LINUX}
  Self.Constraints.MinHeight:= FCharSize.cy * 18;
  {$ELSE}
  Self.Constraints.MinHeight:= FCharSize.cy * 20;
  {$ENDIF}

  //Self.Constraints.MaxWidth:= Self.Constraints.MinWidth;
  //Self.Constraints.MaxHeight:= Self.Constraints.MinHeight;


  with Self do
  begin
    //BorderStyle:= bsDialog;
    BorderIcons:= [biSystemMenu];
    //AutoScroll:= True;
    AutoSize:= True;
    BorderStyle:= bsSizeable;
  end;
  Panel1.AutoSize:= True;
end;

procedure TfrmFontDialogEx.lbxFamilyClick(Sender: TObject);
var
  i: PtrInt = -1;
begin
  LoadFamilyFonts(-1);
  lbxCharsetClick(nil);

  if (lbxCharset.Count > 0) then
  begin
    i:= cbbCharset.Items.IndexOf(lbxCharset.Items[lbxCharset.ItemIndex]);
    if (i <> -1)
      then cbbCharset.ItemIndex:= i
      else cbbCharset.ItemIndex:= 1;
  end;

  SelectFont;
end;

procedure TfrmFontDialogEx.lbxCharsetClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbxCharset.ItemIndex;
  if (i < 0) then exit;
  i := PtrInt(lbxCharset.Items.Objects[i]);
  LoadFamilyFonts(byte(i));
end;

procedure TfrmFontDialogEx.lbxSizesClick(Sender: TObject);
begin
  SelectFont;
end;

procedure TfrmFontDialogEx.lbxStylesClick(Sender: TObject);
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
  if cbbCharset.Itemindex<0 then
    result := ANSI_CHARSET
  else
    result := Byte(PtrInt(cbbCharset.items.Objects[cbbCharset.ItemIndex]));
end;

function TfrmFontDialogEx.GetPitch: integer;
begin
  case cbbPitch.ItemIndex of
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
      if l=lbxFamily then Event := @lbxFamilyClick else
      if l=lbxStyles then Event := @lbxStylesClick else
      if l=lbxCharset then Event := @lbxCharsetClick else
      if l=lbxSizes then Event := @lbxSizesClick;
    end;
    L.OnClick := Event;
  end;
begin
  if Lb<>nil then
    SetEvent(Lb)
  else begin
    SetEvent(lbxFamily);
    SetEvent(lbxStyles);
    SetEvent(lbxCharset);
    SetEvent(lbxSizes);
  end;
end;

procedure TfrmFontDialogEx.SelectFont;
var
  i: integer;
  function GetFontSize(s: string): Integer;
  begin
    i := pos('*',s);
    if i<>0 then
      result := StrToInt(Copy(S, 1, i-1))
    else
      result := StrToInt(s);
  end;
begin
  if (lbxFamily.ItemIndex >= 0) then
    if (lbxCharset.ItemIndex >= 0) then
      if (lbxStyles.ItemIndex >= 0) then
        if (lbxSizes.ItemIndex >= 0) then
        begin
          FSelfFont.Name := lbxFamily.Items[lbxFamily.ItemIndex];
          FSelfFont.CharSet := TFontCharSet(ptrint(lbxCharset.Items.Objects[lbxCharset.ItemIndex]));
          FSelfFont.Size := GetFontSize(lbxSizes.Items[lbxSizes.ItemIndex]);
          i:= PtrInt(lbxStyles.Items.Objects[lbxStyles.ItemIndex]);
          //Self.Caption:= Format('lbxStyles i = %d',[i]);
          FSelfFont.Style:= [];
          if ((i and 1) <> 0) then FSelfFont.Style:= FSelfFont.Style + [fsItalic];
          if ((i and 2) <> 0) then FSelfFont.Style:= FSelfFont.Style + [fsBold];
          if chbUnderLine.Checked then FSelfFont.Style:= FSelfFont.Style + [fsUnderline];
          if chbStrike.Checked then FSelfFont.Style:= FSelfFont.Style + [fsStrikeOut];
          FSelfFont.Color:= clboxFontColor.Selected;
          UpdateFont(FSelfFont);
          //SaveSelection;
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
    grid.Cols[0] := SL;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TfrmFontDialogEx.SaveSelection;
  function doGet(lb: TListbox): string;
  begin
    if lb.itemindex>=0 then
      result := lb.Items[lb.ItemIndex]
    else
      result := '';
  end;
begin
  FCurrentFamily := doGet(lbxFamily);
  FCurrentCharset := doGet(lbxCharset);
  FCurrentStyle := doGet(lbxStyles);
  FCurrentSize := doGet(lbxSizes);
  FCurrentColor:= ColorToString(clboxFontColor.Selected);
end;

procedure TfrmFontDialogEx.RestoreSelection(Sender: TListbox);
  function GetSelection: string;
  begin
    if (Sender.itemindex >= 0) then
      result := Sender.Items[Sender.ItemIndex]
    else
      result := '';
  end;
  function GetCurrent: string;
  begin
    if Sender=lbxFamily then result := FCurrentFamily else
    if Sender=lbxCharset then result := FCurrentCharset else
    if Sender=lbxStyles then result := FCurrentStyle else
    if Sender=lbxSizes then result := FCurrentSize;
  end;
var
  i: Integer;
  s: string;
begin
  s := GetCurrent;
  if GetSelection <> s then
  begin
    i:= Sender.Items.IndexOf(s);
    if (i <> -1)
    then
      begin
        {$ifdef debug}
        debugln('RestoreSelection: listbox=',Sender.Name,' Old=',GetSelection,' New=',S);
        {$endif}
        if (i <> Sender.ItemIndex) then
          Sender.ItemIndex := i;
      end;
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
  lf.lfCharSet := GetCharSet;
  lf.lfFaceName := '';
  case cbbPitch.ItemIndex of
    1: i:=FIXED_PITCH;
    2: i:=VARIABLE_PITCH;
    3: i:=MONO_FONT;
    else
      i:= DEFAULT_PITCH;
  end;

  lf.lfPitchAndFamily:= i;

  {$ifdef debug}
  debugln('LoadFontList: for charset=',CharSetToString(lf.lfcharset));
  {$endif}

  L:= TStringList.create;
  lbxStyles.Clear;
  lbxCharset.Clear;
  lbxSizes.Clear;

  DC := GetDC(0);
  EnableEvents(False, lbxFamily);
  try
    //StartTimer;
    EnumFontFamiliesEX(DC, @lf, @EnumFontsNoDups, ptrint(L), 0);
    //EndTimer;
    L.Sort;
    lbxFamily.Items.Assign(L);
    lbxFamily.Itemindex:= -1;
    
    RestoreSelection(lbxFamily);
    if (lbxFamily.ItemIndex < 0) then begin
      if (lbxFamily.Items.Count > 0) then
        lbxFamily.ItemIndex:= 0;
    end;
    LoadFamilyFonts(-1);
    
    //lblFontFaceList.Caption := format('Fontfaces, found %d, %d ms',[lbxFamily.Items.Count, FTime]);
    lblFontFaceList.Caption := format(cFontFaceList,[lbxFamily.Items.Count]);

  finally
    EnableEvents(True, lbxFamily);
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
  i:= lbxFamily.ItemIndex;
  if (i < 0) then exit;
  
  LoadingCharsets:= (Charset < 0);

  {$ifdef debug}
  Write('LoadFamilyFonts: for family=', lbxFamily.Items[i],' and Charset=');
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
    Lf.lfFaceName := lbxFamily.Items[i];
    Lf.lfCharSet := byte(Charset);
    Lf.lfPitchAndFamily := 0;
    NeedTTF := False;
    EnumFontFamiliesEX(DC, @Lf, @EnumFamilyFonts, ptrint(LCharset), 0);
    // fill charset listbox if necessary
    if LCharset<>nil then begin
      LCharset.Sort;
      EnableEvents(False, lbxCharset);
      lbxCharset.Items.Assign(LCharset);
      lbxCharset.ItemIndex := -1;
      EnableEvents(true, lbxCharset);
    end else begin
      // fill styles listbox
      LStyles.Sort;
      EnableEvents(False, lbxStyles);
      lbxStyles.Items.Assign(LStyles);
      lbxStyles.ItemIndex := -1;
      EnableEvents(true, lbxStyles);
      RestoreSelection(lbxStyles);
      if lbxStyles.ItemIndex<0 then begin
       if lbxStyles.Items.Count>0 then
          lbxStyles.ItemIndex := 0;
      end;
      // fill sizes listbox
      // any raster font size is already there
      if NeedTTF then
        AddScalableSizes;
      LSizes.CustomSort(@CompareSizes);
      EnableEvents(False, lbxSizes);
      lbxSizes.Items.Assign(LSizes);
      lbxSizes.ItemIndex := -1;
      EnableEvents(true, lbxSizes);
      RestoreSelection(lbxSizes);
      if lbxSizes.ItemIndex<0 then begin
        if lbxSizes.Items.Count>0 then
          lbxSizes.ItemIndex := 0;
      end;
    end;
  finally
    if LCharset=nil then begin
      LSizes.Free;
      LStyles.Free;
    end else
      LCharset.Free;
    releaseDC(0, DC);
  end;
  
  if LoadingCharsets then begin
    // make an initial charset selection
    RestoreSelection(lbxCharset);
    if lbxCharset.ItemIndex<0 then begin
      if lbxCharset.Items.Count>0 then
        lbxCharset.ItemIndex := 0;
    end;
  end;
end;

procedure TfrmFontDialogEx.UpdateFont(F: TFont);
begin
  grid.Font := F;
  grid.DefaultRowHeight := grid.canvas.textHeight('Wj') + 5;
end;

procedure TfrmFontDialogEx.FillcbbCharSet;
  procedure Add(Charset: Integer);
  begin
    cbbCharset.Items.AddObject(CharSetToString(CharSet), TObject(ptrint(Charset)));
  end;
begin
  // populate cbbCharset
  cbbCharset.Items.clear;
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

  cbbCharset.ItemIndex:= 1;
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

