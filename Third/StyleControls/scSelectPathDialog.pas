{*******************************************************************}
{                                                                   }
{       Almediadev Visual Component Library                         }
{       StyleControls                                               }
{       Version 4.62                                                }
{                                                                   }
{       Copyright (c) 2014-2020 Almediadev                          }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{       Home:  http://www.almdev.com                                }
{       Support: support@almdev.com                                 }
{                                                                   }
{*******************************************************************}

unit scSelectPathDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, Vcl.ComCtrls,
  scShellControls, scStyledForm;

type
  TscSelPathDlgForm = class(TForm)
    ShellTreeView: TscShellTreeView;
    CreateButton: TscButton;
    OKButton: TscButton;
    CancelButton: TscButton;
    scStyledForm1: TscStyledForm;
    procedure FormCreate(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure ShellTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TscSelectPathDialog = class(TComponent)
  private
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FDialogClientWidth,
    FDialogClientHeight: Integer;
    FTitle: String;
    FDlgFrm: TscSelPathDlgForm;
    FPath: String;
    FShowCreateButton: Boolean;
    FScaled: Boolean;
    FAnimationOnControls: Boolean;
  protected
    procedure SetAnimation;
    procedure Change;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property DialogClientWidth: Integer read FDialogClientWidth write FDialogClientWidth;
    property DialogClientHeight: Integer read FDialogClientHeight write FDialogClientHeight;
    property Path: String read FPath write FPath;
    property Title: String read FTitle write FTitle;
    property Scaled: Boolean read FScaled write FScaled;
    property AnimationOnControls: Boolean
      read FAnimationOnControls write FAnimationOnControls;
    property ShowCreateButton: Boolean read FShowCreateButton write FShowCreateButton;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
   Uses
    scDrawUtils, scDlgStrs;

{$R *.dfm}


procedure TscSelPathDlgForm.CreateButtonClick(Sender: TObject);
var
  NewName: string;
  TreeNode: TTreeNode;
  I: integer;

  function GetUniName: string;
  var
    I: integer;
  begin
    Result := '';
    for I := 1 to 100 do
    begin
      Result := ShellTreeView.Path + '\' + SC_S_NewFolder + IntToStr(I);
      if not DirectoryExists(Result) then
        Exit;
    end;
  end;

begin
  NewName := GetUniName;
  if NewName = '' then Exit;
  if CreateDir(NewName) then
  begin
    if ShellTreeView.Selected <> nil then
    begin
      ShellTreeView.Refresh(ShellTreeView.Selected);
      for I := 0 to ShellTreeView.Selected.Count - 1 do
      begin
        TreeNode := ShellTreeView.Selected.Item[I];
        if ShellTreeView.Folders[TreeNode.AbsoluteIndex].PathName = NewName then
        begin
          TreeNode.Selected := True;
          ShellTreeView.SetFocus;
          TreeNode.EditText;
          ShellTreeView.Path := NewName;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TscSelPathDlgForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewHeight < 400 then
    NewHeight := 400;
  if NewWidth < 400 then
    NewWidth := 400;
end;

procedure TscSelPathDlgForm.FormCreate(Sender: TObject);
begin
  if SC_RTLMODE then
    BidiMode := bdRightToLeft;

  Caption := SC_S_SelectDir;
  OkButton.Caption := SC_S_Msg_OK;
  CancelButton.Caption := SC_S_Msg_Cancel;
  CreateButton.Caption := SC_S_CREATE;
end;

procedure TscSelPathDlgForm.ShellTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if ShellTreeView.SelectedFolder <> nil then
    CreateButton.Enabled := DirectoryExists(ShellTreeView.Path)
  else
    CreateButton.Enabled := False;
end;

constructor TscSelectPathDialog.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationOnControls := False;
  FShowCreateButton := True;
  FDialogClientWidth := 0;
  FDialogClientHeight := 0;
  FScaled := True;
  FTitle := '';
  FPath := '';
end;

procedure TscSelectPathDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TscSelectPathDialog.SetAnimation;
var
  I: Integer;
begin
  if FDlgFrm = nil then Exit;
  for I := 0 to FDlgFrm.ComponentCount - 1 do
  begin
    if FDlgFrm.Components[I] is TscButton then
      TscButton(FDlgFrm.Components[I]).Animation := FAnimationOnControls;
  end;
end;

function TscSelectPathDialog.Execute: Boolean;
var
  IsChange: Boolean;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  FDlgFrm := TscSelPathDlgForm.Create(Application);
  FDlgFrm.CreateButton.Visible := FShowCreateButton;
  SetAnimation;
  if FTitle <> '' then
    FDlgFrm.Caption := FTitle;
  IsChange := False;
  try
    if FDialogClientWidth > 400 then
      FDlgFrm.ClientWidth := FDialogClientWidth;
    if FDialogClientHeight > 400 then
      FDlgFrm.ClientHeight := FDialogClientHeight;

    if (FPath <> '') and DirectoryExists(FPath)
    then
      FDlgFrm.ShellTreeView.Path := FPath
    else
      FDlgFrm.ShellTreeView.ExpandMyComputer;

    Result := (FDlgFrm.ShowModal = mrOk);
    if Result then
    begin
      if FPath <> FDlgFrm.ShellTreeView.Path then
      begin
        FPath := FDlgFrm.ShellTreeView.Path;
        IsChange := True;
      end;
    end;
    if FDlgFrm.scStyledForm1.FDPIChanged then
    begin
      FDialogClientWidth := Round(FDlgFrm.ClientWidth / FDlgFrm.scStyledForm1.ScaleFactor);
      FDialogClientHeight := Round(FDlgFrm.ClientHeight / FDlgFrm.scStyledForm1.ScaleFactor);
    end
    else
    begin
      FDialogClientWidth := FDlgFrm.ClientWidth;
      FDialogClientHeight := FDlgFrm.ClientHeight;
    end;
  finally
    FDlgFrm.Free;
    if IsChange then
      Change;
    if Assigned(FOnClose) then 
      FOnClose(Self);
  end;
end;

end.
