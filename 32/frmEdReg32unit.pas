{

  32-bit registers value editor v2020.4 (Feb/2020)
  =================================================

  Copyright (C) 1996-2020 Ultraplayer aka MvT mishka066 at g m a i l . c o m

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit frmEdReg32unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ActnList, frameEdReg32unit, frameANSI32Unit,
  frameUnicode32unit;

type

  { TfrmEdReg32 }

  TfrmEdReg32 = class(TForm)
    acRevertInitialValue: TAction;
    ActionList1: TActionList;
    bCancel: TButton;
    bOK: TButton;
    bOptions: TButton;
    frUnicode32: TFrameEditUnicode32;
    frANSI32: TFrameEditANSI32;
    frFloatsBE32: TframeEditRegisters32;
    frSignedBE32: TframeEditRegisters32;
    frFloats32: TframeEditRegisters32;
    frUnsigned32: TframeEditRegisters32;
    frSigned32: TframeEditRegisters32;
    frHex32: TframeEditRegisters32;
    LError: TLabel;
    MenuItem1: TMenuItem;
    panelButtons: TPanel;
    panelMainLabels: TPanel;
    panelE: TPanel;
    panelH: TPanel;
    panelL: TPanel;
    panelX: TPanel;
    PopupMenu1: TPopupMenu;
    procedure acRevertInitialValueExecute(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bOptionsClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Reg: TInt32RecAlt;
    FRegName: string;
    FInitValue: LongInt;

    function GetValueString: string;

    procedure HexRegisterValueChanged(RegisterEdit: TRegisterEdit32);
    procedure SignedRegisterValueChanged(RegisterEdit: TRegisterEdit32);
    procedure UnsignedRegisterValueChanged(RegisterEdit: TRegisterEdit32);
    procedure FloatsRegisterValueChanged(RegisterEdit: TRegisterEdit32);
    procedure SignedBERegisterValueChanged(RegisterEdit: TRegisterEdit32);
    procedure FloatsBERegisterValueChanged(RegisterEdit: TRegisterEdit32);
    //
    procedure ANSI32ValueChanged({%H-}ASCIIEdit: TANSIEdit32);
    procedure Unicode32ValueChanged({%H-}UnicodeEdit: TUnicodeEdit32);

    procedure ResetErrors(CallerFrame: Integer);

    procedure SetMainLabels(const E, X, H, L: string); overload;
    procedure SetMainLabels(const Value: string); overload;

    procedure SetRegName(const RegName: string = 'EAX');
    procedure UpdateOthers(CallerFrame: Integer);
    procedure ShowError(const Message: string; Sender: TObject);
  public
    { public declarations }
    procedure Init(RegValue: LongInt; const RegName32: string = 'EAX'); overload;
    procedure Init(RegValue: LongWord; const RegName32: string = 'EAX'); overload;
    procedure Init(const RegValueInt: string; const RegName: string = 'RAX'); overload;
    procedure InitHex(const RegValueHex: string; const RegName: string = 'RAX');

    property ValueString: string read GetValueString;
  end;

var
  frmEdReg32: TfrmEdReg32;

implementation

{$R *.lfm}

const
  clError: TColor = clRed;

{ TfrmEdReg32 }

procedure TfrmEdReg32.FormCreate(Sender: TObject);
begin
  // Create
  FInitValue := 0;

  with frHex32 do begin
    OnRegister32ValueChanged := @HexRegisterValueChanged;
    // EdE.MaxLength := 8;  // Disabled, for some users, who maybe wants old-style full control
    EdX.MaxLength := 4;
    EdH.MaxLength := 2;
    EdL.MaxLength := 2;

    EdE.CharCase := ecUppercase;
    EdX.CharCase := ecUppercase;
    EdH.CharCase := ecUppercase;
    EdL.CharCase := ecUppercase;

    SetLabels('');

    SetHints(
      'Expected 32-bit hex value: 0..FFFFFFFF',
      'Expected 16-bit hex value: 0..FFFF',
      'Expected 8-bit hex value: 0..FF',
      'Expected 8-bit hex value: 0..FF'
    );
  end;

  with frSigned32 do begin
    OnRegister32ValueChanged := @SignedRegisterValueChanged;
    SetLabels(
      '-2G..+2G',       // -2147483648..2147483647
      '-32K..+32K',     // -32768..32767
      '-128..+127',
      '-128..+127'
    );

    SetHints(
      'Expected 32-bit signed value: -2147483648..2147483647',
      'Expected 16-bit signed value: -32768..32767',
      'Expected 8-bit signed value: -128..+127',
      'Expected 8-bit signed value: -128..+127'
    );
  end;

  with frUnsigned32 do begin
    OnRegister32ValueChanged := @UnsignedRegisterValueChanged;
    SetLabels(
      '0 .. +4G',     // 0..4294967295
      '0 .. +64K',    // 0..65535
      '0 .. +255',    // 0..255
      '0 .. +255'     // 0..255
    );
    SetHints(
      'Expected 32-bit unsigned value: 0..4294967295',
      'Expected 16-bit unsigned value: 0..65535',
      'Expected 8-bit unsigned value: 0..255',
      'Expected 8-bit unsigned value: 0..255'
    );
  end;

  with frFloats32 do begin
    OnRegister32ValueChanged := @FloatsRegisterValueChanged;
    EdX.Visible := False;
    EdH.Visible := False;
    EdL.Visible := False;
    SetLabels(
      '1.5e-45 .. 3.4e38',   // 1.5E-45 .. 3.4E38 (Float/Single)
      '',                    // No 16 bit
      '',                    // No high 8 bit
      ''                     // No low 8 bit
    );
    SetHints(
      'Expected 32-bit float value: 1.5E-45 .. 3.4E38',
      '',                    // No 16 bit
      '',                    // No high 8 bit
      ''                     // No low 8 bit
    );
  end;

  with frSignedBE32 do begin
    OnRegister32ValueChanged := @SignedBERegisterValueChanged;
    SetLabels(
      '-2G..+2G',       // -2147483648..2147483647
      '-32K..+32K',     // -32768..32767
      '-128..+127',
      '-128..+127'
    );
    SetHints(
      'Expected 32-bit signed big endian value: -2147483648..2147483647',
      'Expected 16-bit signed big endian value: -32768..32767',
      'Expected 8-bit signed big endian value: -128..+127',
      'Expected 8-bit signed big endian value: -128..+127'
    );
  end;

  with frFloatsBE32 do begin
    OnRegister32ValueChanged := @FloatsBERegisterValueChanged;
    EdX.Visible := False;
    EdH.Visible := False;
    EdL.Visible := False;
    SetLabels(
      '1.5e-45 .. 3.4e38',   // 1.5E-45 .. 3.4E38 (Float/Single)
      '',                    // No 16 bit
      '',                    // No high 8 bit
      ''                     // No low 8 bit
    );
    SetHints(
      'Expected 32-bit float big endian value: 1.5E-45 .. 3.4E38',
      '',                    // No 16 bit
      '',                    // No high 8 bit
      ''                     // No low 8 bit
    );
  end;

  with frANSI32 do begin
    OnANSI32ValueChanged := @ANSI32ValueChanged;

    SetLabels(
      '24-31:',
      '16-23:',
      '8-15:',
      '0-7:'
    );

    SetHints(
      'Expected ANSI char value native: "A".."Z", hex: "\xFF", dec: "\255"'
    );
    LText.Hint := 'ANSI includes all ASCII 0..127 chars and 128..255 current Windows ANSI codepage chars';
  end;

  with frUnicode32 do begin
    OnUnicode32ValueChanged := @Unicode32ValueChanged;

    SetLabels(
      'Bits 16-31:',
      'Bits 0-15:'
    );

    SetHints(
      'Expected Unicode char value native: "A", hex: "\x0000", dec: "\1234"'
    );
  end;

end;

procedure TfrmEdReg32.bOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmEdReg32.bOptionsClick(Sender: TObject);
begin
  PopupMenu1.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmEdReg32.FormActivate(Sender: TObject);
begin
  //{$IFOPT D+}Init($FEEDFACE, 'EAX');{$ENDIF}
end;

procedure TfrmEdReg32.bCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmEdReg32.acRevertInitialValueExecute(Sender: TObject);
begin
  // Action Revert Initial
  Reg.LongInts := FInitValue;
  ResetErrors(-1);
  UpdateOthers(-1); // Update all
end;

procedure TfrmEdReg32.HexRegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 0;
  TheName: string = 'hex';
begin
  with frHex32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueHex32(reE32, Reg.LongInts) then begin
            SetValuesHex(Reg, reE32);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      reX16:
        begin
          if GetValueHex16(reX16, Reg.SmallInts[0]) then begin
            SetValuesHex(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value. %s', [TheName, EdX.Hint]), EdX);
        end;

      reH8:
        begin
          if GetValueHex8(reH8, Reg.Bytes[1]) then begin
            SetValuesHex(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value. %s', [TheName, EdH.Hint]), EdH);
        end;

      reL8:
        begin
          if GetValueHex8(reL8, Reg.Bytes[0]) then begin
            SetValuesHex(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value. %s', [TheName, EdL.Hint]), EdL);
        end;
    end; // case
  end; // with
end;

function TfrmEdReg32.GetValueString: string;
begin
  Result := IntToHex(Reg.LongInts, 8);
end;

procedure TfrmEdReg32.SignedRegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 1;
  TheName: string = 'signed';
begin
  with frSigned32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueSigned32(reE32, Reg.LongInts) then begin
            SetValuesSigned(Reg, reE32);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      reX16:
        begin
          if GetValueSigned16(reX16, Reg.SmallInts[0]) then begin
            SetValuesSigned(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value. %s', [TheName, EdX.Hint]), EdX);
        end;

      reH8:
        begin
          if GetValueSigned8(reH8, Reg.ShortInts[1]) then begin
            SetValuesSigned(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value. %s', [TheName, EdH.Hint]), EdH);
        end;

      reL8:
        begin
          if GetValueSigned8(reL8, Reg.ShortInts[0]) then begin
            SetValuesSigned(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value. %s', [TheName, EdL.Hint]), EdL);
        end;
    end; // case
  end; // with
end;

procedure TfrmEdReg32.UnsignedRegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 2;
  TheName: string = 'unsigned';
begin
  with frUnsigned32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueUnsigned32(reE32, Reg.LongWords) then begin
            SetValuesUnsigned(Reg, reE32);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      reX16:
        begin
          if GetValueUnsigned16(reX16, Reg.Words[0]) then begin
            SetValuesUnsigned(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value. %s', [TheName, EdX.Hint]), EdX);
        end;

      reH8:
        begin
          if GetValueUnsigned8(reH8, Reg.Bytes[1]) then begin
            SetValuesUnsigned(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value. %s', [TheName, EdH.Hint]), EdH);
        end;

      reL8:
        begin
          if GetValueUnsigned8(reL8, Reg.Bytes[0]) then begin
            SetValuesUnsigned(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value. %s', [TheName, EdL.Hint]), EdL);
        end;
    end; // case
  end; // with
end;

procedure TfrmEdReg32.FloatsRegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 3;
  TheName: string = 'float point';
begin
  with frFloats32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueFloat32(reE32, Reg.Float) then begin
            SetValuesFloats(Reg, reE32);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      {
      reX16:
        begin
          if GetValueUnsigned16(reX16, Reg.Words[0]) then begin
            SetValuesUnsigned(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value', [TheName]), EdX);
        end;

      reH8:
        begin
          if GetValueUnsigned8(reH8, Reg.Bytes[1]) then begin
            SetValuesUnsigned(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value', [TheName]), EdH);
        end;

      reL8:
        begin
          if GetValueUnsigned8(reL8, Reg.Bytes[0]) then begin
            SetValuesUnsigned(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value', [TheName]), EdL);
        end;
      }
    end; // case
  end; // with
end;

procedure TfrmEdReg32.SignedBERegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 4;
  TheName: string = 'signed big endian';
var
  X16: SmallInt;
  Sh: ShortInt;
  RegBE: TInt32RecAlt;
begin
  with frSignedBE32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueSigned32(reE32, Reg.LongInts) then begin
            Reg.LongInts := SwapEndian(Reg.LongInts);
            SetValuesSigned(Reg, reE32); // via signed here
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      reX16:
        begin
          X16 := 0; // Make comiper happy
          if GetValueSigned16(reX16, X16) then begin
            // $FEEDFACE -> $EDFE
            RegBE.LongInts := SwapEndian(Reg.LongInts);
            RegBE.SmallInts[0] := X16;
            Reg.LongInts := SwapEndian(RegBE.LongInts);
            SetValuesSignedBE(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value. %s', [TheName, EdX.Hint]), EdX);
        end;

      reH8:
        begin
          Sh := 0; // Make comiper happy
          if GetValueSigned8(reH8, Sh) then begin
            // $FEEDFACE -> $FE
            RegBE.LongInts := SwapEndian(Reg.LongInts);
            RegBE.ShortInts[1] := Sh;
            Reg.LongInts := SwapEndian(RegBE.LongInts);
            SetValuesSignedBE(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value. %s', [TheName, EdH.Hint]), EdH);
        end;

      reL8:
        begin
          Sh := 0; // Make comiper happy
          if GetValueSigned8(reL8, Sh) then begin
            // $FEEDFACE -> $ED
            RegBE.LongInts := SwapEndian(Reg.LongInts);
            RegBE.ShortInts[0] := Sh;
            Reg.LongInts := SwapEndian(RegBE.LongInts);
            SetValuesSignedBE(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value. %s', [TheName, EdL.Hint]), EdL);
        end;
    end; // case
  end; // with
end;

procedure TfrmEdReg32.FloatsBERegisterValueChanged(RegisterEdit: TRegisterEdit32);
const
  Caller = 5;
  TheName: string = 'float point big endian';
begin
  with frFloatsBE32 do begin
    ResetErrors(-1);

    case RegisterEdit of
      reE32:
        begin
          if GetValueFloat32(reE32, Reg.Float) then begin
            Reg.LongInts := SwapEndian(Reg.LongInts); // Change through LongInt
            SetValuesFloatsBE(Reg, reE32);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 32-bit %s value. %s', [TheName, EdE.Hint]), EdE);
        end;

      {
      reX16:
        begin
          if GetValueUnsigned16(reX16, Reg.Words[0]) then begin
            SetValuesUnsigned(Reg, reX16);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse 16-bit %s value', [TheName]), EdX);
        end;

      reH8:
        begin
          if GetValueUnsigned8(reH8, Reg.Bytes[1]) then begin
            SetValuesUnsigned(Reg, reH8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse high 8-bit %s value', [TheName]), EdH);
        end;

      reL8:
        begin
          if GetValueUnsigned8(reL8, Reg.Bytes[0]) then begin
            SetValuesUnsigned(Reg, reL8);
            UpdateOthers(Caller);
          end else ShowError(Format('Error parse low 8-bit %s value', [TheName]), EdL);
        end;
      }
    end; // case
  end; // with
end;

procedure TfrmEdReg32.ANSI32ValueChanged(ASCIIEdit: TANSIEdit32);
const
  Caller = 8;
  TheName: string = 'ANSI';
var
  sError: string;
  L: LongInt;
  B: Byte;
begin
  with frANSI32 do begin
    ResetErrors(-1);

    L := 0; // Make compiler happy
    if GetAllValues(L) then begin
      Reg.LongInts := L;
      UpdateOthers(Caller);
    end else begin
      sError := Format('Error parse %s values.', [TheName]);

      B := 0; // Make compiler happy
      if not StringToByte(Ed1.Text, B) then begin
        Ed1.Color := clError;
        sError := Format('%s %s. ', [sError, Ed1.Hint]);
      end;
      if not StringToByte(Ed2.Text, B) then begin
        Ed2.Color := clError;
        sError := Format('%s %s. ', [sError, Ed2.Hint]);
      end;
      if not StringToByte(Ed3.Text, B) then begin
        Ed3.Color := clError;
        sError := Format('%s %s. ', [sError, Ed3.Hint]);
      end;
      if not StringToByte(Ed4.Text, B) then begin
        Ed4.Color := clError;
        sError := Format('%s %s. ', [sError, Ed4.Hint]);
      end;

      ShowError(sError, nil);
    end;
  end; // with

end;

procedure TfrmEdReg32.Unicode32ValueChanged(UnicodeEdit: TUnicodeEdit32);
const
  Caller = 9;
  TheName: string = 'unicode';
var
  sError: string;
  L: LongInt;
  W: Word;
begin
  with frUnicode32 do begin
    ResetErrors(-1);

    L := 0; // Make compiler happy
    if GetAllValues(L) then begin
      Reg.LongInts := L; // Copy via LongInt
      UpdateOthers(Caller);
    end else begin
      sError := Format('Error parse %s values.', [TheName]);

      W := 0; // Make compiler happy
      if not StringToWord(Ed12.Text, W) then begin
        Ed12.Color := clError;
        sError := Format('%s %s. ', [sError, Ed12.Hint]);
      end;
      if not StringToWord(Ed34.Text, W) then begin
        Ed34.Color := clError;
        sError := Format('%s %s. ', [sError, Ed34.Hint]);
      end;

      ShowError(sError, nil);
    end;

  end; // with
end;

procedure TfrmEdReg32.ResetErrors(CallerFrame: Integer);
begin
  // Reset errors
  bOK.Enabled := True;
  LError.Visible := False;
  LError.Hint := '';

  if CallerFrame <> 0 then
    frHex32.SetEditColors(clDefault);
  if CallerFrame <> 1 then
    frSigned32.SetEditColors(clDefault);
  if CallerFrame <> 2 then
    frUnsigned32.SetEditColors(clDefault);
  if CallerFrame <> 3 then
    frFloats32.SetEditColors(clDefault);
  if CallerFrame <> 4 then
    frSignedBE32.SetEditColors(clDefault);
  if CallerFrame <> 5 then
    frFloatsBE32.SetEditColors(clDefault);
  //if CallerFrame <> 6 then
  //  frFileTime.SetAllEditColor(clDefault);
  //if CallerFrame <> 7 then
  //  frDateTime.SetAllEditColor(clDefault);
  if CallerFrame <> 8 then
    frANSI32.SetAllEditColor(clDefault);
  if CallerFrame <> 9 then
    frUnicode32.SetAllEditColor(clDefault);
end;

procedure TfrmEdReg32.SetMainLabels(const E, X, H, L: string);
begin
  panelE.Caption := E;
  panelX.Caption := X;
  panelH.Caption := H;
  panelL.Caption := L;
end;

procedure TfrmEdReg32.SetMainLabels(const Value: string);
begin
  SetMainLabels(Value, Value, Value, Value);
end;

procedure TfrmEdReg32.SetRegName(const RegName: string = 'EAX');
  procedure SetGPR(const R: string);
  var
    S: string;
  begin
    S := Copy(R, 2, 1); // EAX => A
    SetMainLabels(
      'E' + S + 'X',
      S + 'X',
      S + 'H',
      S + 'L'
    );
  end;

  procedure SetStackRegs(const R: string);
  var
    S: string;
  begin
    S := Copy(R, 2, 2); // ESI => SI
    SetMainLabels(
      'E' + S,
      S,
      'High',
      S + 'L'
    );
  end;

begin
  FRegName := AnsiUpperCase(RegName);

  Caption := Format('Edit Register: %s', [FRegName]);

  if (FRegName = 'EAX')
  or (FRegName = 'EBX')
  or (FRegName = 'ECX')
  or (FRegName = 'EDX') then
    SetGPR(FRegName);

  if (FRegName = 'ESI')
  or (FRegName = 'EDI')
  or (FRegName = 'EBP')
  or (FRegName = 'ESP')
  or (FRegName = 'EIP') then
    SetStackRegs(FRegName);
end;

procedure TfrmEdReg32.UpdateOthers(CallerFrame: Integer);
begin
  // Update all others
  if CallerFrame <> 0 then
    frHex32.SetValuesHex(Reg);
  if CallerFrame <> 1 then
    frSigned32.SetValuesSigned(Reg);
  if CallerFrame <> 2 then
    frUnsigned32.SetValuesUnsigned(Reg);
  if CallerFrame <> 3 then
    frFloats32.SetValuesFloats(Reg);
  if CallerFrame <> 4 then
    frSignedBE32.SetValuesSignedBE(Reg);
  if CallerFrame <> 5 then
    frFloatsBE32.SetValuesFloatsBE(Reg);

  if CallerFrame <> 8 then
    frANSI32.SetAllValuesANSI(Reg.LongInts);
  if CallerFrame <> 9 then
    frUnicode32.SetAllValuesUnicode(Reg.LongInts);
end;

procedure TfrmEdReg32.ShowError(const Message: string; Sender: TObject);
begin
  // Set error to others
  bOK.Enabled := False;
  LError.Caption := Message;
  LError.Visible := True;
  LError.Hint := Message;
  if Sender <> nil then
    (Sender as TEdit).Color := clError;
end;

procedure TfrmEdReg32.Init(RegValue: LongInt; const RegName32: string = 'EAX');
begin
  // [!] Overload (Main #1)
  Reg.LongInts := RegValue;
  FInitValue := RegValue;
  SetRegName(RegName32);
  ResetErrors(-1);
  UpdateOthers(-1); // Update all
end;

procedure TfrmEdReg32.Init(RegValue: LongWord; const RegName32: string);
var
  R: TInt32RecAlt;
begin
  // [!] Overload #2
  R.LongWords := RegValue;
  Init(R.LongInts, RegName32);
end;

procedure TfrmEdReg32.Init(const RegValueInt: string; const RegName: string);
var
  L: LongInt;
begin
  // [!] Overload #3
  if TryStrToInt(RegValueInt, L) then
    Init(L, RegName);
end;

procedure TfrmEdReg32.InitHex(const RegValueHex: string; const RegName: string);
begin
  Init('$' + RegValueHex, RegName);
end;


end.

