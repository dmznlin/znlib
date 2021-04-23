unit ServerItemEditor;

{$I ModLink.inc}

interface

//--------------------------------------------------------------------------------------------------

uses
  { Windows } Windows, Messages,
  { Delphi  } SysUtils, {$IFDEF COMPILER_6_UP} Variants , {$ENDIF COMPILER_6_UP} Classes, Graphics,
              Controls, Forms, Dialogs, StdCtrls,
  { Project } ServerItem;

//--------------------------------------------------------------------------------------------------

type
  TServerItemEditorForm = class(TForm)
    Label1: TLabel;
    AddressEdit: TEdit;
    ValueCheckBox: TCheckBox;
    Label2: TLabel;
    ValueEdit: TEdit;
    Label3: TLabel;
    MinValueEdit: TEdit;
    Label4: TLabel;
    MaxValueEdit: TEdit;
    WriteableCheckBox: TCheckBox;
    OKButton: TButton;
    Button1: TButton;
    procedure OKButtonClick(Sender: TObject);
  private
    FIsNewItem: Boolean;
    FServerItem: PServerItem;
  public
    { Public declarations }
  end;

//--------------------------------------------------------------------------------------------------

var
  ServerItemEditorForm: TServerItemEditorForm;

//--------------------------------------------------------------------------------------------------

function EditServerItem(ServerItem: PServerItem; IsNewItem: Boolean): Boolean;

//--------------------------------------------------------------------------------------------------

implementation

uses
  SysConst, Consts;

//--------------------------------------------------------------------------------------------------

{$R *.DFM}

//--------------------------------------------------------------------------------------------------

function EditServerItem(ServerItem: PServerItem; IsNewItem: Boolean): Boolean;
const
  ItemKinds: array [TItemKind] of string = (
    'Coil',
    'Discrete Input',
    'Holding Register',
    'Input Register'
  );
begin
  with TServerItemEditorForm.Create(nil) do
    try
      FIsNewItem := IsNewItem;
      FServerItem := ServerItem;

      if FIsNewItem then
        Caption := Format('New %s', [ItemKinds[ServerItem^.Kind]])
      else
        Caption := Format('Edit %s', [ItemKinds[ServerItem^.Kind]]);

      with AddressEdit do
      begin
        Text := IntToStr(ServerItem^.Addr);
        ReadOnly := not FIsNewItem;
        if ReadOnly then
          Color := clBtnFace;
      end;

      if ServerItem^.Kind in [ikCoil, ikDiscreteInput] then
      begin
        ValueCheckBox.Checked := Boolean(ServerItem^.Value);
        Label2.Visible := False;
        ValueEdit.Visible := False;
        Label3.Visible := False;
        MinValueEdit.Visible := False;
        Label4.Visible := False;
        MaxValueEdit.Visible := False;
      end
      else
      begin
        ValueEdit.Text := IntToStr(ServerItem^.Value);
        MinValueEdit.Text := IntToStr(ServerItem^.MinValue);
        MaxValueEdit.Text := IntToStr(ServerItem^.MaxValue);
        ValueCheckBox.Visible := False;
      end;
      if ServerItem^.Kind in [ikCoil, ikHoldingRegister] then
        WriteableCheckBox.Checked := ServerItem^.Writeable
      else
        WriteableCheckBox.Visible := False;

      Result := ShowModal = mrOk;

      if Result then
      begin
        ServerItem^.Addr := StrToInt(AddressEdit.Text);
        if ServerItem^.Kind in [ikCoil, ikDiscreteInput] then
          ServerItem^.Value := Ord(ValueCheckBox.Checked)
        else
        begin
          ServerItem^.Value := StrToInt(ValueEdit.Text);
          ServerItem^.MinValue := StrToInt(MinValueEdit.Text);
          ServerItem^.MaxValue := StrToInt(MaxValueEdit.Text);
        end;
        if ServerItem^.Kind in [ikCoil, ikHoldingRegister] then
          ServerItem^.Writeable := WriteableCheckBox.Checked;
      end;
    finally
      Free;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure ValidateNumberInEditBox(AEdit: TEdit; MinValue, MaxValue: Int64);

  // begin of local block --------------------------------------------------------------------------

  procedure Error(ResStringRec: PResStringRec; const Args: array of const);
  begin
    {$IFDEF COMPILER_5_UP}
    raise Exception.CreateResFmt(ResStringRec, Args);
    {$ELSE}
    raise Exception.CreateFmt(LoadResString(ResStringRec), Args);
    {$ENDIF COMPILER_5_UP}
  end;

  procedure InvalidInteger;
  begin
    if AEdit.CanFocus then AEdit.SetFocus;
    Error(@SInvalidInteger, [AEdit.Text]);
  end;

  procedure InvalidRange;
  begin
    if AEdit.CanFocus then AEdit.SetFocus;
    Error(@SOutOfRange, [MinValue, MaxValue]);
  end;

  // end of local block ----------------------------------------------------------------------------

var
  I: Int64;
begin
  try
    I := StrToInt64(AEdit.Text);
    if { (MinValue <> MaxValue) and } ((I < MinValue) or (I > MaxValue)) then
      InvalidRange;
  except
    on E: Exception do
      if E is EConvertError then
      begin
        InvalidInteger;
      end
      else
        raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TServerItemEditorForm.OKButtonClick(Sender: TObject);
begin
  try
    ValidateNumberInEditBox(AddressEdit, 0, High(Word));
    if FServerItem^.Kind in [ikHoldingRegister, ikInputRegister] then
    begin
      ValidateNumberInEditBox(MinValueEdit, 0, High(Word));
      ValidateNumberInEditBox(MaxValueEdit, StrToInt(MinValueEdit.Text), High(Word));
      ValidateNumberInEditBox(ValueEdit, StrToInt(MinValueEdit.Text), StrToInt(MaxValueEdit.Text));
    end;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

end.
