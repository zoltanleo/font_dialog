unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    StaticText1: TStaticText;
    procedure Button1Click(Sender: TObject);
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
  if FontDialog1.Execute then
  begin
    Label1.Font.Assign(FontDialog1.Font);
    StaticText1.Font.Assign(FontDialog1.Font);
  end;
end;

end.

