{ **********************************************************************
    This file is part of the "Inductance and Capacitance Calculator"

    Help form unit

    Author  : Maico Smaniotto
    Created : July, 2020
    Contact : maicosmaniotto@yahoo.com.br

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
unit FHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TFormHelp = class(TForm)
    imgInductorEqw: TImage;
    imgCapacitorEqw: TImage;
    lbInductorTitle: TLabel;
    lbInductorInfo1: TLabel;
    lbInductorInfo2: TLabel;
    lbInductorInfo3: TLabel;
    lbInductorInfo4: TLabel;
    lbInductorInfo5: TLabel;
    lbInductorInfo6: TLabel;
    lbInductorInfo7: TLabel;
    lbCapacitorTitle: TLabel;
    lbCapacitorInfo1: TLabel;
    lbCapacitorInfo2: TLabel;
    lbCapacitorInfo3: TLabel;
    lbCapacitorInfo4: TLabel;
    lbCapacitorInfo5: TLabel;
    lbCapacitorInfo6: TLabel;
    lbCapacitorInfo7: TLabel;
    btClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btCloseClick(Sender: TObject);
  private

  public
    destructor Destroy; override;
  end;

var
  FormHelp: TFormHelp;

implementation

{$R *.lfm}

{ TFormHelp }

procedure TFormHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CaFree;
end;

procedure TFormHelp.btCloseClick(Sender: TObject);
begin
  Self.Close;
end;

destructor TFormHelp.Destroy;
begin
  FormHelp := nil;
  inherited Destroy;
end;

end.

