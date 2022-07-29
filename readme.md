To do:

- [x] Под линукс не меняется cbbCharset.ItemIndex при нажатии в lbxCharset

----------

v.0.0.34

- сделана поправка на возможное значение SelfFont.Size = 0

v.0.0.33

- в edtFontSizeEditingDone для Darwin закомментирован код из-за ошибки "List index (-1) out of bounds" 

v.0.0.32

- избавляемся от промежуточных полей при загрузке параметров шрифта
- удалены процедуры SaveSelection/RestoreSelection

v.0.0.31

- переименованы некоторые компоненты: edtFontFamily, lbxFontFamily, lblFontFamily, lblFontStyles, lbxFontStyles, lblFontSizes, lbxFontSizes, gdSample, lblFontCharset, lbxFontCharset, cbbFontCharset, cbbFontPitch
- изменен TabOrder и TabStop для некоторых контролов

v.0.0.30

- добавлены обработчики для edtFontSize и lbxSize
- изменены Color для pnlComp и clboxFontColor
- изменена высота кнопок под Линукс
- линуксовый GUI разделен на gtk и qt(похожа на винду)

v.0.0.29

- добавлены обработчики для edtFamily и lbxFamily

v.0.0.28

- переименованы панели и скроллбар, заданы их свойства

- "почищен" код от комментариев 

v.0.0.27

- панель с компонентами положена на скроллбох, вдвое уменьшена минимальная высота формы, чтобы можно было уменьша окно для маленьких экранов

v.0.0.26

- правки кода для достижения гибкости отображения окна

v.0.0.25

- ограничение высоты lbxFamily переведены в привязке к высоте шрифта, т.к. ItemHeight под линукс почему-то нулевое

v.0.0.24

- запрет на изменение размеров формы и ограничения на размеры листбоксов

v.0.0.23

- кнопки и остальные компоненты положены на две смежные панели

v.0.0.22

- чистка код от комментариев
- мелкие правки кода

v.0.0.21

- правки кода

- стиль формы сделан bsDialog

v.0.0.20

- редизайн формы (убран scrollbox из-за глюков с отрисовкой)

- коммит в новую ветку branch_0_0_16

v.0.0.19

- добавлен обработчик для edtFamily и изменен обработчик lbxFontSizesClick

v.0.0.18

- добавлен черновой обработчик для edtFontSize и изменен обработчик lbxFamilyClick

v.0.0.17

- удален горизонтальный сплиттер

v.0.0.16

- заданы минимальные размеры для некоторых компонентов

v.0.0.15

- добавлен ufontdlgex_i18n.inc с константамми локализации надписей и заголовкой диалогового окна через `{$DEFINE}`. Для включения/отключения "нужной" локализации необходимо убрать/поставить точку перед символом `$`. По умолчанию (если закомментированы все дефайны) включены константы для английского

v.0.0.14

- добавлен возврат значений шрифта в вызывающую форму

- управление размерами кнопок

v.0.0.13

- свойству Caption кнопки фильтра присвоено значение константы 

- исправлена корректная загрузка имени шрифта по умолчанию в системе

v.0.0.12

- создана проперть SelfFont для передачи свойств шрифта в/из диалог(а)
- сделана загрузка и сохранение параметров SelfFont 

v.0.0.11

- добавлены обработчики кнопок

- структура FCharSize с параметрами шрифта текущей формы вынесена в приватную секцию

v.0.0.10

- мелкие правки на соответствие тeкущего чарсета шрифта в комбе и листбоксе

v.0.0.9

- подправлена процедура фильтрации шрифтов
- введены константы для дальнейшей i18n-локализации

v.0.0.8

- подгонка размеров контролов под Дарвин

v.0.0.7

- коррекция положения горизонтального сплиттера и размеров контроллов с учетом платформы вин/лин

v.0.0.6

- минимальная высота отдельно для линукса

- добавлен горизонтальный сплиттер и задано Top

v.0.0.5

- задана минимальная высота и ширина формы
- контролы вложены в скролл бокс

v.0.0.4

- удален memo (не поддерживает зачеркивание и подчеркивание)

- добавлен обработчик цвета

v.0.0.3

- эксперименты с отображением образца текста
- добавлен memo

v.0.0.2

- первичный дизайн ufontdialogex

v.0.0.1

- скопирован модуль frmMain из последней демки Лазаря с исправленными косяками и переименован в ufontdialogex
