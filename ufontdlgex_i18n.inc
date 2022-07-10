{=============================================================}
{=== This file contains constants to localize the captions ===}
{=== of the dialog box.                                    ===}
{=== Put a point in front of the symbol '$' to cancel the  ===}
{=== action of the directive. The default constants for    ===}
{=== English are used.                                     ===}
{=============================================================}

{$DEFINE RUS_LNG}

const

  {$IFDEF RUS_LNG}
  cTitle = 'Выберите шрифт';
  cFontFaceList = 'Имя шрифта (найдено: %d шт.):';
  cFontStyles = 'Стили шрифта:';
  cFontSize = 'Размер шрифта:';
  cFontEffects = 'Эффекты:';
  cStrikeout = 'Зачеркнутый';
  cUnderline = 'Подчеркнутый';
  cFontColor = 'Цвет шрифта:';
  cFontSample= 'Образец:';
  cFontFilter = 'Фильтр шрифтов';
  cFontCharSet = 'Набор символов текущего шрифта:';
  cBtnOKCaption = 'ОК';
  cBtnCancelCaption = 'Отмена';
  cBtnApplyFilter = 'Применить фильтр';
  {$ELSE}
  cTitle = 'Choose font';
  cFontFaceList = 'Font family (found: %d items):';
  cFontStyles = 'Font style(s):';
  cFontSize = 'Font size:';
  cFontEffects = 'Font effects:';
  cStrikeout = 'StrikeOut';
  cUnderline = 'Underline';
  cFontColor = 'Font color:';
  cFontSample= 'Font sample:';
  cFontFilter = 'Font filter';
  cFontCharSet = 'Current font charset:';
  cBtnOKCaption = 'ОК';
  cBtnCancelCaption = 'Cancel';
  cBtnApplyFilter = 'Apply filter';
  {$ENDIF}


