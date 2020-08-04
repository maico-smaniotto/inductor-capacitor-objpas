{ **********************************************************************
    This file is part of the "Inductance and Capacitance Calculator"

    About form unit

    Author  : Maico Smaniotto
    Created : July, 2020
    Contact : maicosmaniotto@yahoo.com.br

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
unit FAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLIntf;

type
  TFormAbout = class(TForm)
    btClose: TButton;
    lbProgramTitle: TLabel;
    lbCreatorLabel: TLabel;
    lbProjectLabel: TLabel;
    lbCreatorName: TLabel;
    lbProjectLink: TLabel;
    lbContactLabel: TLabel;
    lbContactEmail: TLabel;
    ImageIcon: TImage;
    procedure lbContactEmailClick(Sender: TObject);
    procedure lbProjectLinkClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.lbContactEmailClick(Sender: TObject);
begin
  OpenURL('mailto:' + lbContactEmail.Caption + '?subject=' + lbProgramTitle.Caption + '&body=');
end;

procedure TFormAbout.lbProjectLinkClick(Sender: TObject);
begin
  OpenURL(lbProjectLink.Caption);
end;

procedure TFormAbout.btCloseClick(Sender: TObject);
begin
  Self.Close;
end;

end.

