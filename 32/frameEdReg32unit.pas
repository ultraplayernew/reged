unit frameEdReg32unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Graphics;

type

  { TInt32RecAlt }

  TInt32RecAlt = packed record
    case Integer of
      0: (LongWords: LongWord);                 // 1 x 4 bytes (0..4294967295)
      1: (LongInts: LongInt);                   // 1 x 4 bytes (-2147483648..2147483647)
      2: (Words: array [0..1] of Word);         // 2 x 2 bytes (0..65535)
      3: (SmallInts: array [0..1] of SmallInt); // 2 x 2 bytes (-32768..32767)
      4: (Bytes: array [0..3] of Byte);         // 4 x 1 bytes (0..255)
      5: (ShortInts: array [0..3] of ShortInt); // 4 x 1 bytes (-128..127)
      6: (Float: Single);                       // 1 x 4 bytes (1.5E-45..3.4E38)
    end;


  TRegisterEdit32 = (reE32, reX16, reH8, reL8);
  TOnRegister32ValueChanged = procedure(RegisterEdit: TRegisterEdit32) of object;

  { TframeEditRegisters32 }

  TframeEditRegisters32 = class(TFrame)
    EdE: TEdit;
    EdX: TEdit;
    EdH: TEdit;
    EdL: TEdit;
    LE: TLabel;
    LX: TLabel;
    LH: TLabel;
    LL: TLabel;
    LText: TLabel;
    procedure EdEChange(Sender: TObject);
    procedure EdXChange(Sender: TObject);
    procedure EdHChange(Sender: TObject);
    procedure EdLChange(Sender: TObject);
  private
    { private declarations }
    FOnRegister32ValueChanged: TOnRegister32ValueChanged;
    procedure DoRegister32ValueChanged(RegisterEdit: TRegisterEdit32);
  public
    { public declarations }
    procedure EnableEvents(E, X, H, L: Boolean); overload;
    procedure EnableEvents(Flag: Boolean); overload;

    procedure SetValues(const E, X, H, L: string); overload;
    procedure SetValues(const E, X, H, L: string; DisableEvents: Boolean); overload;
    procedure SetValues(const E, X, H, L: string;
      DisableEvents: Boolean; DontChangeRegisterEdit: TRegisterEdit32); overload;
    procedure SetValuesHex(Reg: TInt32RecAlt);
    procedure SetValuesHex(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);
    procedure SetValuesSigned(Reg: TInt32RecAlt);
    procedure SetValuesSigned(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);
    procedure SetValuesUnsigned(Reg: TInt32RecAlt);
    procedure SetValuesUnsigned(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);
    procedure SetValuesFloats(Reg: TInt32RecAlt);
    procedure SetValuesFloats(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);
    procedure SetValuesSignedBE(Reg: TInt32RecAlt);
    procedure SetValuesSignedBE(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);
    procedure SetValuesFloatsBE(Reg: TInt32RecAlt);
    procedure SetValuesFloatsBE(Reg: TInt32RecAlt; DontChangeRegisterEdit: TRegisterEdit32);

    function GetValue(RegisterEdit: TRegisterEdit32): string;

    function GetValueHex32(RegisterEdit: TRegisterEdit32; var Value: LongInt): Boolean;
    function GetValueHex16(RegisterEdit: TRegisterEdit32; var Value: SmallInt): Boolean;
    function GetValueHex8(RegisterEdit: TRegisterEdit32; var Value: Byte): Boolean;

    function GetValueSigned32(RegisterEdit: TRegisterEdit32; var Value: LongInt): Boolean;
    function GetValueSigned16(RegisterEdit: TRegisterEdit32; var Value: SmallInt): Boolean;
    function GetValueSigned8(RegisterEdit: TRegisterEdit32; var Value: ShortInt): Boolean;

    function GetValueUnsigned32(RegisterEdit: TRegisterEdit32; var Value: LongWord): Boolean;
    function GetValueUnsigned16(RegisterEdit: TRegisterEdit32; var Value: Word): Boolean;
    function GetValueUnsigned8(RegisterEdit: TRegisterEdit32; var Value: Byte): Boolean;

    function GetValueFloat32(RegisterEdit: TRegisterEdit32; var Value: Single): Boolean;

    procedure SetHints(const E, X, H, L: string); overload;
    procedure SetHints(const Value: string); overload;

    procedure SetLabels(const E, X, H, L: string); overload;
    procedure SetLabels(const Value: string); overload;

    procedure SetEditColors(const E, X, H, L: TColor); overload;
    procedure SetEditColors(Value: TColor); overload;

    // Events
    property OnRegister32ValueChanged: TOnRegister32ValueChanged read FOnRegister32ValueChanged
      write FOnRegister32ValueChanged;
  end;

implementation

{$R *.lfm}

function IntToStr(Value: SmallInt): string; overload;
var
  L: Longint;
begin
  L := Value;
  Result := IntToStr(L);
end;

function IntToStr(Value: ShortInt): string; overload;
var
  L: Longint;
begin
  L := Value;
  Result := IntToStr(L);
end;

function IntToStr(Value: LongWord): string; overload;
var
  I64: Int64;
begin
  I64 := Value;
  Result := IntToStr(I64);
end;

function IntToStr(Value: Word): string; overload;
var
  L: Longint;
begin
  L := Value;
  Result := IntToStr(L);
end;

function IntToStr(Value: Byte): string; overload;
var
  L: Longint;
begin
  L := Value;
  Result := IntToStr(L);
end;

function SwapEndian(const Float: Single): Single; overload;
var
  T: TInt32RecAlt;
begin
  T.Float := Float;
  T.LongInts := SwapEndian(T.LongInts);
  Result := T.Float;
end;

function FloatToStrFix(Value: Double): string; overload;
begin
  // This fixing default FloatToStr when user directly changes bytes via Integer.
  // For example, if set int values = -8388607 .. -4194305 and read float, this
  // raises error. So we need to check nan manually
  if Value.SpecialType <> TFloatSpecial.fsNaN then
    Result := FloatToStr(Value)
  else
    Result := 'Nan';
end;

function FloatToStrFix(Value: Single): string; overload;
begin
  // This fixing default FloatToStr when user directly changes bytes via Integer.
  // For example, if set int values = -8388607 .. -4194305 and read float, this
  // raises error. So we need to check nan manually
  if Value.SpecialType <> TFloatSpecial.fsNaN then
    Result := FloatToStr(Value)
  else
    Result := 'Nan';
end;

{ TframeEditRegisters32 }

procedure TframeEditRegisters32.EdEChange(Sender: TObject);
begin
  // E (32 bit) Reg change event
  DoRegister32ValueChanged(reE32);
end;

procedure TframeEditRegisters32.EdXChange(Sender: TObject);
begin
  // X (16 bit) Reg change event
  DoRegister32ValueChanged(reX16);
end;

procedure TframeEditRegisters32.EdHChange(Sender: TObject);
begin
  // H (8 bit) Reg change event
  DoRegister32ValueChanged(reH8);
end;

procedure TframeEditRegisters32.EdLChange(Sender: TObject);
begin
  // L (8 bit) Reg change event
  DoRegister32ValueChanged(reL8);
end;

procedure TframeEditRegisters32.DoRegister32ValueChanged(
  RegisterEdit: TRegisterEdit32);
begin
  if Assigned(FOnRegister32ValueChanged) then
    FOnRegister32ValueChanged(RegisterEdit);
end;

procedure TframeEditRegisters32.EnableEvents(E, X, H, L: Boolean);
begin
  // All registers
  // [!] Overload
  if E then
    EdE.OnChange := @EdEChange
  else
    EdE.OnChange := nil;

  if X then
    EdX.OnChange := @EdXChange
  else
    EdX.OnChange := nil;

  if H then
    EdH.OnChange := @EdHChange
  else
    EdH.OnChange := nil;

  if L then
    EdL.OnChange := @EdLChange
  else
    EdL.OnChange := nil;
end;

procedure TframeEditRegisters32.EnableEvents(Flag: Boolean);
begin
  // All registers
  // [!] Overload
  EnableEvents(Flag, Flag, Flag, Flag);
end;

procedure TframeEditRegisters32.SetValues(const E, X, H, L: string);
begin
  EdE.Text := E;
  EdX.Text := X;
  EdH.Text := H;
  EdL.Text := L;
end;

procedure TframeEditRegisters32.SetValues(const E, X, H, L: string;
  DisableEvents: Boolean);
begin
  if DisableEvents then begin
    EnableEvents(False);
    try
      SetValues(E, X, H, L);
    finally
      EnableEvents(True);
    end;
  end else
    SetValues(E, X, H, L);
end;

procedure TframeEditRegisters32.SetValues(const E, X, H, L: string;
  DisableEvents: Boolean; DontChangeRegisterEdit: TRegisterEdit32);
begin
  if DisableEvents then begin
    EnableEvents(False);
    try
      if DontChangeRegisterEdit <> reE32 then
        EdE.Text := E;
      if DontChangeRegisterEdit <> reX16 then
        EdX.Text := X;
      if DontChangeRegisterEdit <> reH8 then
        EdH.Text := H;
      if DontChangeRegisterEdit <> reL8 then
        EdL.Text := L;
    finally
      EnableEvents(True);
    end;
  end else begin
    if DontChangeRegisterEdit <> reE32 then
      EdE.Text := E;
    if DontChangeRegisterEdit <> reX16 then
      EdX.Text := X;
    if DontChangeRegisterEdit <> reH8 then
      EdH.Text := H;
    if DontChangeRegisterEdit <> reL8 then
      EdL.Text := L;
  end;
end;

procedure TframeEditRegisters32.SetValuesHex(Reg: TInt32RecAlt);
begin
  SetValues(
    IntToHex(Reg.LongWords, 8),
    IntToHex(Reg.Words[0], 4),
    IntToHex(Reg.Bytes[1], 2),
    IntToHex(Reg.Bytes[0], 2),
    True
  );
end;

procedure TframeEditRegisters32.SetValuesHex(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
begin
  SetValues(
    IntToHex(Reg.LongWords, 8),
    IntToHex(Reg.Words[0], 4),
    IntToHex(Reg.Bytes[1], 2),
    IntToHex(Reg.Bytes[0], 2),
    True,
    DontChangeRegisterEdit
  );
end;

procedure TframeEditRegisters32.SetValuesSigned(Reg: TInt32RecAlt);
begin
  SetValues(
    IntToStr(Reg.LongInts),
    IntToStr(Reg.SmallInts[0]),
    IntToStr(Reg.ShortInts[1]),
    IntToStr(Reg.ShortInts[0]),
    True
  );
end;

procedure TframeEditRegisters32.SetValuesSigned(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
begin
  SetValues(
    IntToStr(Reg.LongInts),
    IntToStr(Reg.SmallInts[0]),
    IntToStr(Reg.ShortInts[1]),
    IntToStr(Reg.ShortInts[0]),
    True,
    DontChangeRegisterEdit
  );
end;

procedure TframeEditRegisters32.SetValuesUnsigned(Reg: TInt32RecAlt);
begin
  SetValues(
    IntToStr(Reg.LongWords),
    IntToStr(Reg.Words[0]),
    IntToStr(Reg.Bytes[1]),
    IntToStr(Reg.Bytes[0]),
    True
  );
end;

procedure TframeEditRegisters32.SetValuesUnsigned(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
begin
  SetValues(
    IntToStr(Reg.LongWords),
    IntToStr(Reg.Words[0]),
    IntToStr(Reg.Bytes[1]),
    IntToStr(Reg.Bytes[0]),
    True,
    DontChangeRegisterEdit
  );
end;

procedure TframeEditRegisters32.SetValuesFloats(Reg: TInt32RecAlt);
begin
  SetValues(
    FloatToStrFix(Reg.Float),
    '',
    '',
    '',
    True
  );
end;

procedure TframeEditRegisters32.SetValuesFloats(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
begin
  SetValues(
    FloatToStrFix(Reg.Float),
    '',
    '',
    '',
    True,
    DontChangeRegisterEdit
  );
end;

procedure TframeEditRegisters32.SetValuesSignedBE(Reg: TInt32RecAlt);
var
  RegBE: TInt32RecAlt;
begin
  // [!] Overload
  RegBE.LongInts := SwapEndian(Reg.LongInts);
  SetValues(
    IntToStr(RegBE.LongInts),
    IntToStr(RegBE.SmallInts[0]),
    IntToStr(RegBE.ShortInts[1]),
    IntToStr(RegBE.ShortInts[0]),
    True
  );
end;

procedure TframeEditRegisters32.SetValuesSignedBE(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
var
  RegBE: TInt32RecAlt;
begin
  // [!] Overload
  RegBE.LongInts := SwapEndian(Reg.LongInts);
  SetValues(
    IntToStr(RegBE.LongInts),
    IntToStr(RegBE.SmallInts[0]),
    IntToStr(RegBE.ShortInts[1]),
    IntToStr(RegBE.ShortInts[0]),
    True,
    DontChangeRegisterEdit
  );
end;

procedure TframeEditRegisters32.SetValuesFloatsBE(Reg: TInt32RecAlt);
begin
  SetValues(
    FloatToStrFix(SwapEndian(Reg.Float)),
    '',
    '',
    '',
    True
  );
end;

procedure TframeEditRegisters32.SetValuesFloatsBE(Reg: TInt32RecAlt;
  DontChangeRegisterEdit: TRegisterEdit32);
begin
  SetValues(
    FloatToStrFix(SwapEndian(Reg.Float)),
    '',
    '',
    '',
    True,
    DontChangeRegisterEdit
  );
end;

function TframeEditRegisters32.GetValue(RegisterEdit: TRegisterEdit32): string;
begin
  Result := '';
  case RegisterEdit of
    reE32: Result := EdE.Text;
    reX16: Result := EdX.Text;
    reH8: Result := EdH.Text;
    reL8: Result := EdL.Text;
  end;
end;

function TframeEditRegisters32.GetValueHex32(RegisterEdit: TRegisterEdit32;
  var Value: LongInt): Boolean;
var
  S: string;
begin
  // Ret 0..FFFFFFFF
  // with Edit's string length fixed 8, no range checks needed
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt('$' + S, Value) then
    Result := True;
end;

function TframeEditRegisters32.GetValueHex16(RegisterEdit: TRegisterEdit32;
  var Value: SmallInt): Boolean;
var
  S: string;
  L: LongInt;
begin
  // Ret 0..FFFF
  // with Edit's string length fixed 4, no range checks needed
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt('$' + S, L) then begin
    Value := SmallInt(L);
    Result := True;
  end;
end;

function TframeEditRegisters32.GetValueHex8(RegisterEdit: TRegisterEdit32;
  var Value: Byte): Boolean;
var
  S: string;
  L: LongInt;
begin
  // Ret 0..FF
  // with Edit's string length fixed 2, no range checks needed
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt('$' + S, L) then begin
    Value := Byte(L);
    Result := True;
  end;
end;

function TframeEditRegisters32.GetValueSigned32(RegisterEdit: TRegisterEdit32;
  var Value: LongInt): Boolean;
var
  S: string;
  I64: Int64;
begin
  // Ret -2147483648..2147483647
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt64(S, I64) then begin
    Value := LongInt(I64);
    if (I64 >= -2147483648) and (I64 <= 2147483647) then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueSigned16(RegisterEdit: TRegisterEdit32;
  var Value: SmallInt): Boolean;
var
  S: string;
  L: LongInt;
begin
  // Ret -32768..32767
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt(S, L) then begin
    Value := SmallInt(L);
    if (L >= -32768) and (L <= 32767) then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueSigned8(RegisterEdit: TRegisterEdit32;
  var Value: ShortInt): Boolean;
var
  S: string;
  L: LongInt;
  B: Byte;
begin
  // Ret -128..127
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToInt(S, L) then begin
    B := Byte(L);
    Value := ShortInt(B);
    if (L >= -128) and (L <= 127) then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueUnsigned32(RegisterEdit: TRegisterEdit32;
  var Value: LongWord): Boolean;
var
  S: string;
  QW: QWord;
begin
  // Ret 0..4294967295
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToQWord(S, QW) then begin
    Value := LongWord(QW);
    if QW <= 4294967295 then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueUnsigned16(RegisterEdit: TRegisterEdit32;
  var Value: Word): Boolean;
var
  S: string;
  DW: LongWord;
begin
  // Ret 0..65535
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToDWord(S, DW) then begin
    Value := Word(DW);
    if DW <= 65535 then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueUnsigned8(RegisterEdit: TRegisterEdit32;
  var Value: Byte): Boolean;
var
  S: string;
  DW: LongWord;
begin
  // Ret 0..255
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  if TryStrToDWord(S, DW) then begin
    Value := Byte(DW);
    if DW <= 255 then // Check out of range
      Result := True;
  end;
end;

function TframeEditRegisters32.GetValueFloat32(RegisterEdit: TRegisterEdit32;
  var Value: Single): Boolean;
var
  S: string;
  FS: TFormatSettings;
begin
  // No range checks needed
  // Update 20/02/2020.
  // CE has self global FormatSettings ".", and  that  blocks  "."  ->  ","
  // convetsion  logics.  So,  this  is  local  FormatSettings  to  prevent
  // conversion errors.
  Result := False;
  S := Trim(GetValue(RegisterEdit));
  S := StringReplace(S, '.', ',', [rfReplaceAll]);
  FS.DecimalSeparator := ',';
  if TryStrToFloat(S, Value, FS) then
    Result := True;
end;

procedure TframeEditRegisters32.SetHints(const E, X, H, L: string);
begin
  // [!] Overload
  EdE.Hint := E;
  EdX.Hint := X;
  EdH.Hint := H;
  EdL.Hint := L;
end;

procedure TframeEditRegisters32.SetHints(const Value: string);
begin
  // [!] Overload
  SetHints(Value, Value, Value, Value);
end;

procedure TframeEditRegisters32.SetLabels(const E, X, H, L: string);
begin
  // [!] Overload
  LE.Caption := E;
  LX.Caption := X;
  LH.Caption := H;
  LL.Caption := L;
end;

procedure TframeEditRegisters32.SetLabels(const Value: string);
begin
  // [!] Overload
  SetLabels(Value, Value, Value, Value);
end;

procedure TframeEditRegisters32.SetEditColors(const E, X, H, L: TColor);
begin
  // [!] Overload
  EdE.Color := E;
  EdX.Color := X;
  EdH.Color := H;
  EdL.Color := L;
end;

procedure TframeEditRegisters32.SetEditColors(Value: TColor);
begin
  // [!] Overload
  SetEditColors(Value, Value, Value, Value);
end;


end.



