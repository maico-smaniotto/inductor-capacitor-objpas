{ **********************************************************************
    This file is part of the "Inductance and Capacitance Calculator"

    Main form unit

    Author   : Maico Smaniotto
    Created  : July, 2020
    Contact  : maicosmaniotto@yahoo.com.br
    Language : Object Pascal
    Compiler : FreePascal v3.2.0 up
    Requires : Lazarus Component Library (LCL)

    Material Design Icons made by Google
    available at "https://www.flaticon.com/authors/google"

    Help/Info icons by Freepik - "http://www.freepik.com"
    available at "https://www.flaticon.com"

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, ExtCtrls, Clipbrd, ElectricalUtils, FHelp, FAbout;

type
  { Defines the direction/way of conversion, used for both the capacitor and the inductor:
    cwReactanceToValue -> To convert an Reactance value to Capacitance/Inductance
    cwValueToReactance -> To convert Capacitance/Inductance to an Reactance value }
  TConversionWay = (cwReactanceToValue, cwValueToReactance);

  TFormMain = class(TForm)
    edInductorResultVal: TEdit;
    lbInductorInput: TLabel;
    lbInductorFrequency: TLabel;
    lbInductorResult: TLabel;
    edCapacitorResultVal: TEdit;
    lbCapacitorInput: TLabel;
    lbCapacitorFrequency: TLabel;
    lbCapacitorResult: TLabel;
    lbInductor: TLabel;
    lbCapacitor: TLabel;
    cbInductorInputUnit: TComboBox;
    cbInductorFrequencyUnit: TComboBox;
    cbInductorResultUnit: TComboBox;
    cbCapacitorFrequencyUnit: TComboBox;
    cbCapacitorInputUnit: TComboBox;
    cbCapacitorResultUnit: TComboBox;
    ImageInductor: TImage;
    ImageCapacitor: TImage;
    edInductorFrequencyVal: TEdit;
    edInductorInputVal: TEdit;
    edCapacitorFrequencyVal: TEdit;
    edCapacitorInputVal: TEdit;
    btHelp: TSpeedButton;
    btAbout: TSpeedButton;
    ImageListButtons: TImageList;
    btInductorAlternate: TBitBtn;
    btInductorCopy: TBitBtn;
    btCapacitorAlternate: TBitBtn;
    btCapacitorCopy: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btInductorCopyClick(Sender: TObject);
    procedure btCapacitorCopyClick(Sender: TObject);
    procedure btInductorAlternateClick(Sender: TObject);
    procedure btCapacitorAlternateClick(Sender: TObject);
    procedure InductorInputChange(Sender: TObject);
    procedure CapacitorInputChange(Sender: TObject);
    procedure InputKeyPress(Sender: TObject; var Key: char);
    procedure cbInductorResultUnitChange(Sender: TObject);
    procedure cbCapacitorResultUnitChange(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
    procedure btAboutClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { Result of the Inductor conversion }
    FInductorResult: Real;
    { Actual direction/way of conversion (Impedance to Inductance OR Inductance to Impedance) }
    FInductorConversion: TConversionWay;

    { Result of the Capacitor conversion }
    FCapacitorResult: Real;
    { Actual direction/way of conversion (Impedance to Capacitance OR Capacitance to Impedance) }
    FCapacitorConversion: TConversionWay;

    { Methods to arrange the components on screen according to the conversion way }
    procedure SetInductorConversion(AValue: TConversionWay);
    procedure SetCapacitorConversion(AValue: TConversionWay);

    { Conversion routines }
    procedure CalculateInductor;
    procedure CalculateCapacitor;
    procedure DisplayInductorResult(AUnit: String);
    procedure DisplayCapacitorResult(AUnit: String);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FInductorConversion  := cwReactanceToValue;
  FCapacitorConversion := cwReactanceToValue;
  FInductorResult      := -1;
  FCapacitorResult     := -1;

  cbInductorFrequencyUnit.Items.Text  := GetFrequencyUnits;
  edInductorFrequencyVal.Text         := '60';
  cbInductorFrequencyUnit.Text        := HERTZ;
  cbCapacitorFrequencyUnit.Items.Text := GetFrequencyUnits;
  edCapacitorFrequencyVal.Text        := '60';
  cbCapacitorFrequencyUnit.Text       := HERTZ;

  SetInductorConversion(FInductorConversion);
  SetCapacitorConversion(FCapacitorConversion);

  edInductorInputVal.Text   := '';
  cbInductorInputUnit.Text  := OHM;
  edCapacitorInputVal.Text  := '';
  cbCapacitorInputUnit.Text := OHM;
end;

procedure TFormMain.btInductorCopyClick(Sender: TObject);
begin
  Clipboard.AsText := FormatValueForInterchange(edInductorResultVal.Text, cbInductorResultUnit.Text);
end;

procedure TFormMain.btCapacitorCopyClick(Sender: TObject);
begin
  Clipboard.AsText := FormatValueForInterchange(edCapacitorResultVal.Text, cbCapacitorResultUnit.Text);
end;

procedure TFormMain.btInductorAlternateClick(Sender: TObject);
var
  ResultVal            : String;
  ResultUnit           : String;
  InputValChangeEvent  : TNotifyEvent;
  InputUnitChangeEvent : TNotifyEvent;
begin
  ResultVal  := edInductorResultVal.Text;
  ResultUnit := cbInductorResultUnit.Text;

  if FInductorConversion = cwReactanceToValue then
    SetInductorConversion(cwValueToReactance)
  else
    SetInductorConversion(cwReactanceToValue);

  InputValChangeEvent  := edInductorInputVal.OnChange;
  InputUnitChangeEvent := cbInductorInputUnit.OnChange;
  try
    edInductorInputVal.OnChange  := nil;
    cbInductorInputUnit.OnChange := nil;
    edInductorInputVal.Text      := ResultVal;
    cbInductorInputUnit.Text     := ResultUnit;
  finally
    edInductorInputVal.OnChange  := InputValChangeEvent;
    cbInductorInputUnit.OnChange := InputUnitChangeEvent;
  end;
  CalculateInductor;
end;

procedure TFormMain.btCapacitorAlternateClick(Sender: TObject);
var
  ResultVal            : String;
  ResultUnit           : String;
  InputValChangeEvent  : TNotifyEvent;
  InputUnitChangeEvent : TNotifyEvent;
begin
  ResultVal  := edCapacitorResultVal.Text;
  ResultUnit := cbCapacitorResultUnit.Text;

  if FCapacitorConversion = cwReactanceToValue then
    SetCapacitorConversion(cwValueToReactance)
  else
    SetCapacitorConversion(cwReactanceToValue);

  InputValChangeEvent  := edCapacitorInputVal.OnChange;
  InputUnitChangeEvent := cbCapacitorInputUnit.OnChange;
  try
    edCapacitorInputVal.OnChange  := nil;
    cbCapacitorInputUnit.OnChange := nil;
    edCapacitorInputVal.Text      := ResultVal;
    cbCapacitorInputUnit.Text     := ResultUnit;
  finally
    edCapacitorInputVal.OnChange  := InputValChangeEvent;
    cbCapacitorInputUnit.OnChange := InputUnitChangeEvent;
  end;
  CalculateCapacitor;
end;

procedure TFormMain.InductorInputChange(Sender: TObject);
begin
  CalculateInductor;
end;

procedure TFormMain.CapacitorInputChange(Sender: TObject);
begin
  CalculateCapacitor;
end;

procedure TFormMain.InputKeyPress(Sender: TObject; var Key: char);
begin
  // Allows only numbers and comma
  // Comma treatment is done here (allowing only one comma)
  // Any other non-numeric character is treated setting the NumbersOnly property to True
  if (Key <> ',') or ((Key = ',') and (Pos(',', TEdit(Sender).Text) > 0)) then
    TEdit(Sender).NumbersOnly := True
  else
    TEdit(Sender).NumbersOnly := False;
end;

procedure TFormMain.cbInductorResultUnitChange(Sender: TObject);
begin
  DisplayInductorResult(TComboBox(Sender).Text);
end;

procedure TFormMain.cbCapacitorResultUnitChange(Sender: TObject);
begin
  DisplayCapacitorResult(TComboBox(Sender).Text);
end;

procedure TFormMain.btHelpClick(Sender: TObject);
begin
  if FormHelp = nil then
    FormHelp := TFormHelp.Create(nil);

  FormHelp.Show;
end;

procedure TFormMain.btAboutClick(Sender: TObject);
var
  FormAbout: TFormAbout;
begin
  FormAbout := TFormAbout.Create(nil);
  FormAbout.ShowModal;
  FormAbout.Free;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 112 then
    btHelp.Click;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  DisplayInductorResult('');
  DisplayCapacitorResult('');
end;

procedure TFormMain.SetInductorConversion(AValue: TConversionWay);
begin
  FInductorConversion := AValue;
  case AValue of
    cwReactanceToValue:
    begin
      lbInductorInput.Caption         := 'Reatância';
      cbInductorInputUnit.Items.Text  := GetReactanceUnits;
      lbInductorResult.Caption        := 'Indutância';
      cbInductorResultUnit.Items.Text := GetInductanceUnits;
    end;
    cwValueToReactance:
    begin
      lbInductorInput.Caption         := 'Indutância';
      cbInductorInputUnit.Items.Text  := GetInductanceUnits;
      lbInductorResult.Caption        := 'Reatância';
      cbInductorResultUnit.Items.Text := GetReactanceUnits;
    end;
  end;
end;

procedure TFormMain.SetCapacitorConversion(AValue: TConversionWay);
begin
  FCapacitorConversion := AValue;
  case AValue of
    cwReactanceToValue:
    begin
      lbCapacitorInput.Caption         := 'Reatância';
      cbCapacitorInputUnit.Items.Text  := GetReactanceUnits;
      lbCapacitorResult.Caption        := 'Capacitância';
      cbCapacitorResultUnit.Items.Text := GetCapacitanceUnits;
    end;
    cwValueToReactance:
    begin
      lbCapacitorInput.Caption         := 'Capacitância';
      cbCapacitorInputUnit.Items.Text  := GetCapacitanceUnits;
      lbCapacitorResult.Caption        := 'Reatância';
      cbCapacitorResultUnit.Items.Text := GetReactanceUnits;
    end;
  end;
end;

procedure TFormMain.CalculateInductor;
var
  Frequency,
  InputValue: Real;
begin
  try
    Frequency  := MultiplyUnit(StrToFloat(edInductorFrequencyVal.Text), cbInductorFrequencyUnit.Text);
    InputValue := MultiplyUnit(StrToFloat(edInductorInputVal.Text), cbInductorInputUnit.Text);

    case FInductorConversion of
      cwReactanceToValue:
        FInductorResult := ReactanceToInductance(InputValue, Frequency);
      cwValueToReactance:
        FInductorResult := InductanceToReactance(InputValue, Frequency);
    end;
  except
    FInductorResult := -1;
  end;

  DisplayInductorResult('');
end;

procedure TFormMain.CalculateCapacitor;
var
  Frequency,
  InputValue: Real;
begin
  try
    Frequency  := MultiplyUnit(StrToFloat(edCapacitorFrequencyVal.Text), cbCapacitorFrequencyUnit.Text);
    InputValue := MultiplyUnit(StrToFloat(edCapacitorInputVal.Text), cbCapacitorInputUnit.Text);

    case FCapacitorConversion of
      cwReactanceToValue:
        FCapacitorResult := ReactanceToCapacitance(InputValue, Frequency);
      cwValueToReactance:
        FCapacitorResult := CapacitanceToReactance(InputValue, Frequency);
    end;
  except
    FCapacitorResult := -1;
  end;

  DisplayCapacitorResult('');
end;

procedure TFormMain.DisplayInductorResult(AUnit: String);
var
  UnitChangeEvent : TNotifyEvent;
  ResultVal       : Double;
begin
  if AUnit = '' then
    case FInductorConversion of
      cwReactanceToValue:
      begin
        if fInductorResult > 0 then
        begin
          AUnit := GetProperPrefix(FInductorResult) + HENRY;
          // If there is no inductance unit with the returned prefix, then considers the base unit HENRY
          if Pos(AUnit, GetInductanceUnits) < 1 then
            AUnit := HENRY;
        end
        else // Zero or negative value, then shows the base unit HENRY
          AUnit := HENRY;
      end;
      cwValueToReactance:
      begin
        if fInductorResult > 0 then
        begin
          AUnit := GetProperPrefix(FInductorResult) + OHM;
          // If there is no reactance unit with the returned prefix, then considers the base unit OHM
          if Pos(AUnit, GetReactanceUnits) < 1 then
            AUnit := OHM;
        end
        else // Zero or negative value, then shows the base unit OHM
          AUnit := OHM;
      end;
    end;

  UnitChangeEvent := cbInductorResultUnit.OnChange;
  try
    cbInductorResultUnit.OnChange := nil;
    cbInductorResultUnit.Text     := AUnit;
  finally
    cbInductorResultUnit.OnChange := UnitChangeEvent;
  end;

  if FInductorResult >= 0 then
  begin
    //// Formats to 10 decimal places and converts to float again to remove trailing zeroes
    //edInductorResultVal.Text := FloatToStr(StrToFloat(Format('%.10f', [DivideUnit(FInductorResult, AUnit)])));

    // Divides the value by the unit
    ResultVal := DivideUnit(FInductorResult, AUnit);
    if ResultVal < 1E6 then
    begin
      // Crops the number to 6 decimal places
      ResultVal := Round(ResultVal * 1E6) / 1E6;
      edInductorResultVal.Text := FloatToStr(ResultVal);
    end
    else // If the value is greater than or equal 1E6 then it is shown in scientific notation
      edInductorResultVal.Text := Format('%.7e', [ResultVal]);
  end
  else // Negative value is not shown
    edInductorResultVal.Text := '';
end;

procedure TFormMain.DisplayCapacitorResult(AUnit: String);
var
  UnitChangeEvent : TNotifyEvent;
  ResultVal       : Double;
begin
  if AUnit = '' then
    case FCapacitorConversion of
      cwReactanceToValue:
      begin
        if fCapacitorResult > 0 then
        begin
          AUnit := GetProperPrefix(FCapacitorResult) + FARAD;
          // If there is no capacitance unit with the returned prefix, then considers the base unit FARAD
          if Pos(AUnit, GetCapacitanceUnits) < 1 then
            AUnit := FARAD;
        end
        else // Zero or negative value, then shows the base unit FARAD
          AUnit := FARAD;
      end;
      cwValueToReactance:
      begin
        if fCapacitorResult > 0 then
        begin
          AUnit := GetProperPrefix(FCapacitorResult) + OHM;
          // If there is no reactance unit with the returned prefix, then considers the base unit OHM
          if Pos(AUnit, GetReactanceUnits) < 1 then
            AUnit := OHM;
        end
        else // Zero or negative value, then shows the base unit OHM
          AUnit := OHM;
      end;
    end;

  UnitChangeEvent := cbCapacitorResultUnit.OnChange;
  try
    cbCapacitorResultUnit.OnChange := nil;
    cbCapacitorResultUnit.Text     := AUnit;
  finally
    cbCapacitorResultUnit.OnChange := UnitChangeEvent;
  end;

  //if FCapacitorResult < 0 then
  //  // Negative value is not shown
  //  edCapacitorResultVal.Text := ''
  //else                           // Formats to 10 decimal places and converts to float again to remove trailing zeroes
  //  edCapacitorResultVal.Text := FloatToStr(StrToFloat(Format('%.10f', [DivideUnit(FCapacitorResult, AUnit)])));

  if FCapacitorResult >= 0 then
  begin
    // Divides the value by the unit
    ResultVal := DivideUnit(FCapacitorResult, AUnit);
    if ResultVal < 1E6 then
    begin
      // Crops the number to 6 decimal places
      ResultVal := Round(ResultVal * 1E6) / 1E6;
      edCapacitorResultVal.Text := FloatToStr(ResultVal);
    end
    else // If the value is greater than or equal 1E6 then it is shown in scientific notation
      edCapacitorResultVal.Text := Format('%.7e', [ResultVal]);
  end
  else // Negative value is not shown
    edCapacitorResultVal.Text := '';
end;

end.

