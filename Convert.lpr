{ **********************************************************************
    This file is part of the "Inductance and Capacitance Calculator"

    Program entry point

    Author   : Maico Smaniotto
    Created  : July, 2020
    Contact  : maicosmaniotto@yahoo.com.br
    Language : Object Pascal
    Compiler : FreePascal v3.2.0 up
    Requires : Lazarus Component Library (LCL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
program Convert;

{$mode objfpc}{$H+}
{$WARN 5044 off : Symbol "$1" is not portable}
{$WARN 5024 off : Parameter "$1" not used}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, FMain, ElectricalUtils, FAbout, FHelp;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.MainFormOnTaskBar := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

