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

  MILI_HENRY  = PREFIX_MILI  + HENRY;
  MICRO_HENRY = PREFIX_MICRO + HENRY;
  NANO_HENRY  = PREFIX_NANO  + HENRY;

  MILI_FARAD  = PREFIX_MILI  + FARAD;
  MICRO_FARAD = PREFIX_MICRO + FARAD;
  NANO_FARAD  = PREFIX_NANO  + FARAD;

  KILO_HERTZ  = PREFIX_KILO + HERTZ;
  MEGA_HERTZ  = PREFIX_MEGA + HERTZ;
  GIGA_HERTZ  = PREFIX_GIGA + HERTZ;

function StringExists(constref AStr: String; constref AArray: array of String): Boolean;

{ Functions that return the list of units to fill the comboboxes }
function GetFrequencyUnits: String;
function GetReactanceUnits: String;
function GetInductanceUnits: String;
function GetCapacitanceUnits: String;

{ Returns the prefix of the unit, if it is prefixed, otherwise returns an empty string
  Ex: 'kHz', returns 'k' }
function GetUnitPrefix(AUnit: String): String;

{ Returns the multiple value of the unit accordingly to its prefix
  Ex: 'kHz', prefix k equals 10E3 }
function GetUnitMultiple(AUnit: String): Real;

{ Returns the most appropriate prefix for a given value, using engineering notation }
function GetProperPrefix(AValue: Real): String;

{ Multiplies a value by the prefix weight of the given unit
  Ex: 5 kHz = 5000 Hz
      MultiplyUnit(5, 'kHz'); returns 5000 }
function MultiplyUnit(AValue: Real; AUnit: String): Real;

{ Divides a value by the prefix weight of the given unit
  Ex: 5000 Hz = 5 kHz
      DivideUnit(5000, 'kHz'); returns 5 }
function DivideUnit(AValue: Real; AUnit: String): Real;

{ Formats output for interchange with simulation softwares:
  Replaces comma by period, utilizes only the unit prefix and replaces 'μ' by 'u' }
function FormatOutputForInterchange(AValue: String; AUnit: String): String;

{ Conversion functions }
function ReactanceToInductance(AReactance, AFrequency: Real): Real;
function InductanceToReactance(AInductance, AFrequency: Real): Real;
function ReactanceToCapacitance(AReactance, AFrequency: Real): Real;
function CapacitanceToReactance(ACapacitance, AFrequency: Real): Real;

implementation

function StringExists(constref AStr: String; constref AArray: array of String): Boolean;
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

function GetFrequencyUnits: String;
begin
  Result := HERTZ      + sLineBreak +
            KILO_HERTZ + sLineBreak +
            MEGA_HERTZ + sLineBreak +
            GIGA_HERTZ;
end;

function GetReactanceUnits: String;
begin
  Result := OHM      + sLineBreak +
            KILO_OHM + sLineBreak +
            MEGA_OHM + sLineBreak +
            GIGA_OHM;
end;

function GetInductanceUnits: String;
begin
  Result := HENRY       + sLineBreak +
            MILI_HENRY  + sLineBreak +
            MICRO_HENRY + sLineBreak +
            NANO_HENRY;
end;

function GetCapacitanceUnits: String;
begin
  Result := FARAD       + sLineBreak +
            MILI_FARAD  + sLineBreak +
            MICRO_FARAD + sLineBreak +
            NANO_FARAD;
end;

function GetUnitPrefix(AUnit: String): String;
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

function GetUnitMultiple(AUnit: String): Real;
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

function MultiplyUnit(AValue: Real; AUnit: String): Real;
begin
  Result := AValue * GetUnitMultiple(AUnit);
end;

function DivideUnit(AValue: Real; AUnit: String): Real;
begin
  Result := AValue / GetUnitMultiple(AUnit);
end;

function FormatOutputForInterchange(AValue: String; AUnit: String): String;
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

