{ **********************************************************************
    This file is part of the "Inductance and Capacitance Calculator"

    Electrical utilities and routines

    Author  : Maico Smaniotto
    Created : July, 2020
    Contact : maicosmaniotto@yahoo.com.br

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
unit ElectricalUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  { Unit prefixes }
  PREFIX_KILO  = 'k';
  PREFIX_MEGA  = 'M';
  PREFIX_GIGA  = 'G';
  PREFIX_MILI  = 'm';
  PREFIX_MICRO = 'μ';
  PREFIX_NANO  = 'n';

  { Base units }
  OHM        = 'Ω';
  HENRY      = 'H';
  FARAD      = 'F';
  HERTZ      = 'Hz';
  BASE_UNITS : array of String = (OHM, HENRY, FARAD, HERTZ);

  { Derived/prefixed units }
  KILO_OHM    = PREFIX_KILO + OHM;
  MEGA_OHM    = PREFIX_MEGA + OHM;
  GIGA_OHM    = PREFIX_GIGA + OHM;
  REACTANCE_UNITS : array [0..3] of String = (OHM, KILO_OHM, MEGA_OHM, GIGA_OHM);

  MILI_HENRY  = PREFIX_MILI  + HENRY;
  MICRO_HENRY = PREFIX_MICRO + HENRY;
  NANO_HENRY  = PREFIX_NANO  + HENRY;
  INDUCTANCE_UNITS : array [0..3] of String = (HENRY, MILI_HENRY, MICRO_HENRY, NANO_HENRY);

  MILI_FARAD  = PREFIX_MILI  + FARAD;
  MICRO_FARAD = PREFIX_MICRO + FARAD;
  NANO_FARAD  = PREFIX_NANO  + FARAD;
  CAPACITANCE_UNITS : array [0..3] of String = (FARAD, MILI_FARAD, MICRO_FARAD, NANO_FARAD);

  KILO_HERTZ  = PREFIX_KILO + HERTZ;
  MEGA_HERTZ  = PREFIX_MEGA + HERTZ;
  GIGA_HERTZ  = PREFIX_GIGA + HERTZ;
  FREQUENCY_UNITS : array [0..3] of String = (HERTZ, KILO_HERTZ, MEGA_HERTZ, GIGA_HERTZ);

function StringExists(const AStr: String; const AArray: array of String): Boolean;

{ Returns the list of units as text (each element separated by a line break) }
function GetUnitsAsText(const AUnits: array of String): String;

{ Returns the prefix of the unit, if it is prefixed, otherwise returns an empty string
  Ex: 'kHz', returns 'k' }
function GetUnitPrefix(const AUnit: String): String;

{ Returns the multiple value of the unit accordingly to its prefix
  Ex: 'kHz', prefix k equals 10E3 }
function GetUnitMultiple(const AUnit: String): Real;

{ Returns the most appropriate prefix for a given value, using engineering notation }
function GetProperPrefix(AValue: Real): String;

{ Multiplies a value by the prefix weight of the given unit
  Ex: 5 kHz = 5000 Hz
      MultiplyUnit(5, 'kHz'); returns 5000 }
function MultiplyUnit(AValue: Real; const AUnit: String): Real;

{ Divides a value by the prefix weight of the given unit
  Ex: 5000 Hz = 5 kHz
      DivideUnit(5000, 'kHz'); returns 5 }
function DivideUnit(AValue: Real; const AUnit: String): Real;

{ Formats a value to given decimal places }
function FormatValue(AValue: Double; ADecimalPlaces: Integer): String;

{ Formats output for interchange with simulation softwares:
  Replaces comma by period, utilizes only the unit prefix and replaces 'μ' by 'u' }
function FormatOutputForInterchange(const AValue: String; const AUnit: String): String;

{ Conversion functions }
function ReactanceToInductance(AReactance, AFrequency: Real): Real;
function InductanceToReactance(AInductance, AFrequency: Real): Real;
function ReactanceToCapacitance(AReactance, AFrequency: Real): Real;
function CapacitanceToReactance(ACapacitance, AFrequency: Real): Real;

implementation

uses Math;

function StringExists(const AStr: String; const AArray: array of String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(AArray) to High(AArray) do
    if AStr = AArray[i] then
    begin
      Result := True;
      Exit;
    end;
end;

function GetUnitsAsText(const AUnits: array of String): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(AUnits) to High(AUnits) do
  begin
    if Result <> '' then
      Result := Result + sLineBreak;
    Result := Result + AUnits[i];
  end;
end;

function GetUnitPrefix(const AUnit: String): String;
var
  UStr: UnicodeString;
begin
  Result := '';

  // If it is not a base unit, then it is a prefixed unit
  if (AUnit <> '') and not StringExists(AUnit, BASE_UNITS) then
  begin
    // Converts to UnicodeString because symbols like "micro" and "omega" occupy more than one character in ANSI encoding
    UStr := UnicodeString(AUnit);
    // Gets the first character/symbol
    Result := String(Copy(UStr, 1, 1));
  end;
end;

function GetUnitMultiple(const AUnit: String): Real;
var
  Prefix: String;
begin
  // Initializes default value for units without prefix
  Result := 1;

  Prefix := GetUnitPrefix(AUnit);
  if Prefix <> '' then
  begin
    // Prefixed unit
    case Prefix of
      PREFIX_MILI  : Result := 1E-3;
      PREFIX_MICRO : Result := 1E-6;
      PREFIX_NANO  : Result := 1E-9;
      PREFIX_KILO  : Result := 1E3;
      PREFIX_MEGA  : Result := 1E6;
      PREFIX_GIGA  : Result := 1E9;
    else
      // Unrecognized prefix
      raise Exception.Create('Prefixo "' + Prefix + '" não reconhecido.');
    end
  end;
end;

function GetProperPrefix(AValue: Real): String;
begin
  Result := '';
  if AValue = 0 then Exit;

  //if AValue > 1E-9 then
  Result := PREFIX_NANO;
  if AValue > 1E-6 then
      Result := PREFIX_MICRO;
  if AValue > 1E-3 then
      Result := PREFIX_MILI;
  if AValue > 1 then
      Result := '';
  if AValue > 1E3 then
      Result := PREFIX_KILO;
  if AValue > 1E6 then
      Result := PREFIX_MEGA;
  if AValue > 1E9 then
      Result := PREFIX_GIGA;
end;

function MultiplyUnit(AValue: Real; const AUnit: String): Real;
begin
  Result := AValue * GetUnitMultiple(AUnit);
end;

function DivideUnit(AValue: Real; const AUnit: String): Real;
begin
  Result := AValue / GetUnitMultiple(AUnit);
end;

function FormatValue(AValue: Double; ADecimalPlaces: Integer): String;
var
  ExpoPrecision: Int64;
begin
  if AValue < 1000000 then
  begin
    ExpoPrecision := Round(Power(10, ADecimalPlaces));

    // Crops the number to given decimal places
    AValue := Double(Round(AValue * ExpoPrecision)) / ExpoPrecision;
    Result := FloatToStr(AValue);
  end
  else // Greater than or equal 1 000 000, output in scientific notation
    Result := Format('%.' + IntToStr(ADecimalPlaces + 1) + 'e', [AValue]);
end;

function FormatOutputForInterchange(const AValue: String; const AUnit: String): String;
begin
  Result := StringReplace(AValue, ',', '.', []) + StringReplace(GetUnitPrefix(AUnit), 'μ', 'u', []);
end;

function ReactanceToInductance(AReactance, AFrequency: Real): Real;
begin
  Result := AReactance / (2 * Pi * AFrequency);
end;

function InductanceToReactance(AInductance, AFrequency: Real): Real;
begin
  Result := 2 * Pi * AFrequency * AInductance;
end;

function ReactanceToCapacitance(AReactance, AFrequency: Real): Real;
begin
  Result := 1 / (2 * Pi * AFrequency * AReactance);
end;

function CapacitanceToReactance(ACapacitance, AFrequency: Real): Real;
begin
  Result := 1 / (2 * Pi * AFrequency * ACapacitance);
end;

end.

