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

unit scOpenFileDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ComCtrls, scShellControls, Vcl.ImgList, Vcl.Mask, scStyledForm, scDialogs;

type
  TscOpenFileDlgForm = class(TForm)
    ToolBarPanel: TscPanel;
    scLabel1: TscLabel;
    ShellBox: TscShellComboBox;
    BackButton: TscButton;
    ImageList1: TImageList;
    LevelUpButton: TscButton;
    NewFolderButton: TscButton;
    ViewMenuButton: TscButton;
    scGalleryMenu1: TscGalleryMenu;
    scStyledForm1: TscStyledForm;
    DesktopButton: TscButton;
    LibraryButton: TscButton;
    PCButton: TscButton;
    NetworkButton: TscButton;
    scLabel2: TscLabel;
    scLabel3: TscLabel;
    FileNameEdit: TscEdit;
    FilterComboBox: TscFilterComboBox;
    OKButton: TscButton;
    CancelButton: TscButton;
    ClientPanel: TscPanel;
    FileListView: TscShellListView;
    ImageList125: TImageList;
    ImageList150: TImageList;
    ImageList200: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure scGalleryMenu1ItemClick(Sender: TObject);
    procedure LevelUpButtonClick(Sender: TObject);
    procedure NewFolderButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure FileListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FileListViewDblClick(Sender: TObject);
    procedure FileNameEditKeyPress(Sender: TObject; var Key: Char);
    procedure OKButtonClick(Sender: TObject);
    procedure FilterComboBoxChange(Sender: TObject);
    procedure FileListViewKeyPress(Sender: TObject; var Key: Char);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure scStyledForm1ChangeScale(AScaleFactor: Double);
    procedure FormResize(Sender: TObject);
  private
    FButtonImages: TImageList;
    procedure InitShellButtons;
    procedure ShellButtonClick(Sender: TObject);
    procedure FLVPathChange(Sender: TObject);
  public
    FolderHistory: TList;
    StopAddToHistory: Boolean;
    SaveMode: Boolean;
    OverwritePromt: Boolean;
    FileName: String;
    DefaultExt: String;
    CheckFileExists: Boolean;
    FOnFolderChange: TNotifyEvent;
    FOnCreateDialog: TNotifyEvent;
    destructor Destroy; override;
    procedure InitHistory;
    procedure InitControls;
  end;

  TscShellListViewStyle = (scvsIcon, scvsSmallIcon, scvsList, scvsReport,
    scvsSmallThumbnails, scvsThumbnails, scvsBigThumbnails, scvsVeryBigThumbnails);

  TscOpenDialog = class(TComponent)
  private
    FShowThumbnails: Boolean;
    FOnShow: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FShowHiddenFiles: Boolean;
    FOverwritePromt: Boolean;
    FDefaultExt: String;
    FDialogClientWidth, FDialogClientHeight: Integer;
    FOnFolderChange: TNotifyEvent;
    FCheckFileExists: Boolean;
    FMultiSelection: Boolean;
    FTitle: String;
    FOnChange: TNotifyEvent;
    FInitialDir: String;
    FFilterIndex: Integer;
    FFiles: TStringList;
    FScaled: Boolean;
    FAnimationOnControls: Boolean;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure SetFileName(const Value: String);
  protected
    FFileName: String;
    FFilter: String;
    FSaveMode: Boolean;
    FDlgFrm: TscOpenFileDlgForm;
    procedure SetAnimation;
    procedure Change;
    procedure CreateForm; virtual;
    procedure InitForm; virtual;
    procedure SaveForm; virtual;
  public
    ListViewStyle: TscShellListViewStyle;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property Files: TStringList read FFiles;
    function GetSelectedFile: String;
  published
    property AnimationOnControls: Boolean
      read FAnimationOnControls write FAnimationOnControls;
    property ShowThumbnails: Boolean read
      FShowThumbnails write FShowThumbnails;
    property ShowHiddenFiles: Boolean
      read FShowHiddenFiles write FShowHiddenFiles;
    property OverwritePromt: Boolean read FOverwritePromt write FOverwritePromt;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property DialogClientWidth: Integer read FDialogClientWidth write FDialogClientWidth;
    property DialogClientHeight: Integer read FDialogClientHeight write FDialogClientHeight;
    property CheckFileExists: Boolean read FCheckFileExists write  FCheckFileExists;
    property MultiSelection: Boolean read FMultiSelection write FMultiSelection;
    property Title: string read GetTitle write SetTitle;
    property InitialDir: String read FInitialDir write FInitialDir;
    property Filter: String read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex;
    property FileName: String read FFileName write SetFileName;
    property Scaled: Boolean read FScaled write FScaled;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  TscSaveDialog = class(TscOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation
   Uses Winapi.ShlObj, WinApi.ShellApi, scDrawUtils, System.TypInfo, System.UITypes;
{$R *.dfm}

procedure TscOpenFileDlgForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewHeight < 450 then
    NewHeight := 450;
  if NewWidth < 600 then
    NewWidth := 600;
end;

procedure TscOpenFileDlgForm.FormCreate(Sender: TObject);
begin
  if SC_RTLMODE then
    BidiMode := bdRightToLeft;

  DefaultExt := '';
  FileName := '';
  CheckFileExists := False;
  OverwritePromt := False;
  SaveMode := False;
  FolderHistory := TList.Create;
  StopAddToHistory := False;
  OKButton.Caption := SC_S_Open;
  Caption := SC_S_FileOpen;
  CancelButton.Caption := SC_S_Cancel;
  scLabel1.Caption := SC_S_Folder;
  scLabel2.Caption := SC_S_FileName;
  scLabel3.Caption := SC_S_FileType;
  BackButton.Hint := SC_S_GoToLastFolderVisited_Hint;
  LevelUpButton.Hint := SC_S_UpOneLevel_Hint;
  NewFolderButton.Hint := SC_S_CreateNewFolder_Hint;
  ViewMenuButton.Hint := SC_S_ViewMenu_Hint;
  if IsWindowsXP then
    FileListView.ThumbnailOptions.Enabled := False;
  FileListView.OnPathChanged := FLVPathChange;
  // initbuttons
  InitShellButtons;
  // for inherited dialogs
  if Assigned(FOnCreateDialog) then
    FOnCreateDialog(Self);

end;

procedure TscOpenFileDlgForm.FormResize(Sender: TObject);
begin
  FileNameEdit.Top := OkButton.Top;
  FilterComboBox.Top := CancelButton.Top;
  FilterComboBox.Width := FileNameEdit.Width;
end;

procedure TscOpenFileDlgForm.InitControls;
begin
  if not SaveMode and (Pos('Save', scLabel1.Caption) > 0) then
  begin
    scLabel1.Caption := 'Look &in:';
  end;
  if SaveMode then
  begin
    OKButton.Caption := SC_S_Save;
    Caption := SC_S_FileSave;
  end;
  if not FileListView.ThumbnailOptions.Enabled or IsWindowsXP then
  begin
    scGalleryMenu1.Items.Delete(scGalleryMenu1.Items.Count - 1);
    scGalleryMenu1.Items.Delete(scGalleryMenu1.Items.Count - 1);
    scGalleryMenu1.Items.Delete(scGalleryMenu1.Items.Count - 1);
    scGalleryMenu1.Items.Delete(scGalleryMenu1.Items.Count - 1);
  end;
  InitHistory;
end;

procedure TscOpenFileDlgForm.BackButtonClick(Sender: TObject);
var
  ID: PItemIDList;
begin
  if FolderHistory.Count > 1 then
  begin
    StopAddToHistory := True;
    ID := PItemIDList(FolderHistory.Items[FolderHistory.Count - 2]);
    FileListView.TreeUpdate(ID);
    FolderHistory.Delete(FolderHistory.Count - 2);
    StopAddToHistory := False;
  end;
end;

destructor TscOpenFileDlgForm.Destroy;
begin
  if FButtonImages <> nil then
    FButtonImages.Free;
  FolderHistory.Clear;
  FolderHistory.Free;
  inherited;
end;

procedure TscOpenFileDlgForm.FileListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if ExtractFileName(FileListView.GetSelectedFile) <> '' then
  begin
    FileNameEdit.Text := ExtractFileName(FileListView.GetSelectedFile);
  end;
  if Assigned(FOnFolderChange) then FOnFolderChange(Self);
end;

procedure TscOpenFileDlgForm.FileListViewDblClick(Sender: TObject);
var
  S, FN: String;
begin
  FN := FileListView.GetSelectedFile;
  if FN <> '' then
  begin
    if OverwritePromt and SaveMode then
    begin
      FN := ExtractFileName(FN);
      S := Format(SC_S_Replace,[FN]);
      if scMessageDlg(S, mtWarning, [mbOk, mbCancel], 0) = mrOk then
      begin
        FileName := FileListView.GetSelectedFile;
        ModalResult := mrOk;
      end;
    end
    else
    begin
      FileName := FileListView.GetSelectedFile;
      ModalResult := mrOk;
    end;
  end;
end;

procedure TscOpenFileDlgForm.FileListViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) and (FileListView.GetSelectedFile <> '') then
    OKButtonClick(Sender);
end;

procedure TscOpenFileDlgForm.FileNameEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    if Pos('*', FileNameEdit.Text) <> 0
    then
      FileListView.Mask := FileNameEdit.Text
    else
    begin
      if Pos('\', FileNameEdit.Text) <> 0 then
      begin
        if DirectoryExists(FileNameEdit.Text) then
          FileListView.Path := FileNameEdit.Text;
      end
      else
      begin
        OKButtonClick(Sender);
      end;
     end;
  end;
end;

procedure TscOpenFileDlgForm.FilterComboBoxChange(Sender: TObject);

function GetFileExt: String;
begin
  if (Pos('.*', FilterComboBox.Mask) = 0)
  then
    begin
      if Pos(';', FilterComboBox.Mask) > 0
      then
        Result := ExtractFileExt(Copy(FilterComboBox.Mask, 1,
                 Pos(';', FilterComboBox.Mask)-1))
      else
        Result := ExtractFileExt(FilterComboBox.Mask);
    end
  else
    Result := '';
end;

var
  Ext, Ext2, S: String;
  i: Integer;
begin
  FileListView.Mask := FilterComboBox.Mask;
  if not Self.SaveMode then Exit;
  Ext := GetFileExt;
  if (Ext <> '') and (FileNameEdit.Text <> '') then
  begin
    Ext2 := ExtractFileExt(FileNameEdit.Text);
    if Ext2 = '' then
      S := FileNameEdit.Text + Ext
   else
   begin
     S := FileNameEdit.Text;
     i := Pos(Ext2, S);
     Delete(S, i, Length(Ext2));
     S := S + Ext;
   end;
      FileNameEdit.Text := S;
  end;
end;

procedure TscOpenFileDlgForm.FLVPathChange(Sender: TObject);
var
  ID: PItemIDList;
begin
  if not StopAddToHistory then
  begin
    ID := CopyPIDL(FileListView.RootFolder.AbsoluteID);
    if (FolderHistory.Count = 0) or
       (ID <> PItemIDList(FolderHistory.Items[FolderHistory.Count - 1]))
    then
      FolderHistory.Add(PItemIDList(ID));
  end;
  with FileListView do
  begin
    if Items.Count <> 0 then
      ItemFocused := Items[0];
  end;
end;

procedure TscOpenFileDlgForm.InitHistory;
var
  ID: PItemIDList;
begin
  ID := CopyPIDL(FileListView.RootFolder.AbsoluteID);
  if (FolderHistory.Count = 0) or
     (ID <> PItemIDList(FolderHistory.Items[FolderHistory.Count - 1]))
  then
    FolderHistory.Add(PItemIDList(ID));
end;

procedure TscOpenFileDlgForm.InitShellButtons;

  procedure InitBtn(Folder: TscRootFolder);
  var
    FButton: TscButton;
    I: Integer;
    S: String;
  begin
    FButton := nil;
    case Folder of
      rfDesktop: FButton := DesktopButton;
      rfMyComputer: FButton := PCButton;
      rfNetwork: FButton := NetworkButton;
      rfPersonal: FButton := LibraryButton;
    end;
    if FButton <> nil then
    begin
      GetRootInfo(Folder, S, I);
      FButton.Caption := S;
      FButton.ImageIndex := I;
      FButton.Images := FButtonImages;
      FButton.Tag := Ord(Folder);
      FButton.OnClick := ShellButtonClick;
    end;
  end;

var
  FLargeImages: Integer;
  FileInfo: TSHFileInfo;
begin
  FButtonImages := TImageList.Create(Self);
  FButtonImages.ShareImages := True;
  FLargeImages := SHGetFileInfo('C:\',
    0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if FLargeImages > 0 then
    FButtonImages.Handle := FLargeImages;
  InitBtn(rfDesktop);
  InitBtn(rfPersonal);
  InitBtn(rfMyComputer);
  InitBtn(rfNetwork);
end;

procedure TscOpenFileDlgForm.LevelUpButtonClick(Sender: TObject);
begin
  FileListView.Back;
end;

procedure TscOpenFileDlgForm.NewFolderButtonClick(Sender: TObject);
var
  NewName: string;
  I: integer;
  S: String;

  function GetUniName: string;
  var
    I: integer;
  begin
    Result := '';
    S := '';
    for I := 1 to 100 do
    begin
      S := SC_S_NewFolder + IntToStr(I);
      Result := FileListView.GetSelectedPath + '\' + S;
      if not DirectoryExists(Result) then
        Exit;
    end;
  end;

begin
  NewName := GetUniName;
  if NewName = '' then Exit;
  if CreateDir(NewName) then
  begin
    FileListView.RootChanged;
    Application.ProcessMessages;
    for I := 0 to FileListView.Items.Count - 1 do
    begin
      if FileListView.Folders[I].DisplayName = S then
      begin
        FileListView.Items[I].EditCaption;
        Break;
      end;
    end;
  end;
end;

procedure TscOpenFileDlgForm.OKButtonClick(Sender: TObject);

function GetFileExt: String;
begin
  if DefaultExt <> ''
  then
    Result := '.' + DefaultExt
  else
  if (Pos('.*', FilterComboBox.Mask) = 0)
  then
    begin
      if Pos(';', FilterComboBox.Mask) > 0
      then
        Result := ExtractFileExt(Copy(FilterComboBox.Mask, 1,
                 Pos(';', FilterComboBox.Mask)-1))
      else
        Result := ExtractFileExt(FilterComboBox.Mask);
    end
  else
    Result := '';
end;


var
  S, S1, S2, S3, FN: String;
begin
  S3 := '';

  if (FileListView.GetSelectedFile = '') and CheckFileExists and not SaveMode and
     (FileNameEdit.Text <> '')
  then
    begin
      S := FileListView.GetSelectedPath;
      if S[Length(S)] <> '\' then S := S + '\';
      S1 := S + FileNameEdit.Text;
      if ExtractFileExt(S1) = ''
      then
        S3 := S1 + GetFileExt
       else
         S3 := S1;
      if not FileExists(S3) then Exit;
    end;


  if FileNameEdit.Text = '' then Exit;

  if SaveMode
  then
    begin
      S1 := FileListView.GetSelectedPath;
      if (Pos('*', FileNameEdit.Text) = 0) and (S1 <> '')
      then
        begin
          S := S1;
          if S[Length(S)] <> '\' then S := S + '\';
          S1 := S + FileNameEdit.Text;
          if ExtractFileExt(S1) = ''
          then
            S3 := S1 + GetFileExt
          else
            S3 := S1;
          if OverwritePromt and FileExists(S3)
          then
            begin
              FN := FileNameEdit.Text;
              S2 := Format(SC_S_Replace, [FN]);
              if scMessageDlg(S2, mtWarning,  [mbOk, mbCancel], 0) = mrOk
              then
                begin
                  FileName := S1;
                  ModalResult := mrOk;
                end;
             end
           else
             begin
               FileName := S1;
               ModalResult := mrOk;
             end;
        end;
    end
  else
    begin
      if CheckFileExists
      then
        begin
          if FileListView.GetSelectedFile = ''
          then
            FileName := S3
          else
            FileName := FileListView.GetSelectedFile;
          if FileExists(FileName) then ModalResult := mrOk else FileName := '';
        end
      else
        begin
          if FileListView.GetSelectedFile <> ''
          then
            FileName := FileListView.GetSelectedFile
          else
            begin
              S := FileListView.GetSelectedPath;
              if S[Length(S)] <> '\' then
              S := S + '\';
              FileName := S + FileNameEdit.Text;
              if ExtractFileExt(FileName) = ''
              then
                FileName := FileName + GetFileExt;
            end;
          ModalResult := mrOk;
        end;
    end;
end;


procedure TscOpenFileDlgForm.scGalleryMenu1ItemClick(Sender: TObject);
begin
  case scGalleryMenu1.ItemIndex of
    0: FileListView.ViewStyle := vsList;
    1: FileListView.ViewStyle := vsReport;
    2: FileListView.ViewStyle := vsSmallIcon;
    3:
      begin
        FileListView.ViewStyle := vsIcon;
        FileListView.ThumbnailOptions.Enabled := False;
      end;
  end;
  if not IsWindowsXP then
  begin
    if(scGalleryMenu1.ItemIndex < 4) then
      FileListView.UpdateThumbnails
    else
    begin
      FileListView.ThumbnailOptions.Enabled := True;
      case scGalleryMenu1.ItemIndex of
        4: FileListView.ThumbnailOptions.SetValues(True, 48, 48);
        5: FileListView.ThumbnailOptions.SetValues(True, 96, 96);
        6: FileListView.ThumbnailOptions.SetValues(True, 128, 128);
        7: FileListView.ThumbnailOptions.SetValues(True, 256, 256);
      end;
      FileListView.ViewStyle := vsIcon;
      FileListView.UpdateThumbnails;
    end;
    FileListView.Repaint;
  end;
end;

procedure TscOpenFileDlgForm.scStyledForm1ChangeScale(AScaleFactor: Double);
var
  IL: TCustomImageList;
begin
  ShellBox.Top := ToolBarPanel.Height div 2 - ShellBox.Height div 2;
  IL := ImageList1;
  if AScaleFactor >= 2 then
    IL := ImageList200
  else
  if AScaleFactor >= 1.5 then
    IL := ImageList150
  else
  if AScaleFactor >= 1.25 then
    IL := ImageList125;
  scGalleryMenu1.Images := IL;
  BackButton.Images := IL;
  NewFolderButton.Images := IL;
  LevelUpButton.Images := IL;
  ViewMenuButton.Images := IL;
end;

procedure TscOpenFileDlgForm.ShellButtonClick(Sender: TObject);
var
  F: TscShellFolder;
  NewPIDL: PItemIDList;
begin
  if (TscButton(Sender).Tag = Ord(rfPersonal)) and not IsWindowsXP then
  begin
    NewPIDL := GetIdList_Libraries;
    if NewPIDL <> nil then
      FileListView.SetPathFromID(NewPIDL)
    else
      FileListView.SetRootDirectly(GetEnumName(TypeInfo(TscRootFolder), TscButton(Sender).Tag));
  end
  else
    FileListView.SetRootDirectly(GetEnumName(TypeInfo(TscRootFolder), TscButton(Sender).Tag));
  F := FileListView.RootFolder;
  ShellBox.SetPathFromID(F.AbsoluteID);
end;


constructor TscOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationOnControls := False;
  FScaled := True;
  FShowThumbnails := True;
  FShowHiddenFiles := False;
  FOverwritePromt := False;
  FDefaultExt := '';
  FDlgFrm := nil;
  FCheckFileExists := True;
  FFiles := TStringList.Create;
  FMultiSelection := False;
  FSaveMode := False;
  FTitle := '';
  FInitialDir := '';
  FFilter := 'All files|*.*';
  FFilterIndex := 1;
  FFileName := '';
  ListViewStyle := scvsList;
  FDialogClientWidth := 0;
  FDialogClientHeight := 0;
end;

destructor TscOpenDialog.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TscOpenDialog.SetAnimation;
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

procedure TscOpenDialog.CreateForm;
begin

end;

procedure TscOpenDialog.InitForm;
begin
  SetAnimation;
end;

procedure TscOpenDialog.SaveForm;
begin

end;

function TscOpenDialog.GetSelectedFile: String;
begin
  if FDlgFrm <>  nil
  then
    Result := FDlgFrm.FileListView.GetSelectedFile
  else
    Result := '';
end;

procedure TscOpenDialog.SetFileName;
begin
  FFileName := Value;
  if FDlgFrm <> nil then FDlgFrm.FileNameEdit.Text := Value;
end;

function TscOpenDialog.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TscOpenDialog.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TscOpenDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TscOpenDialog.Execute: Boolean;
var
  FW, FH: Integer;
  Ext1, Ext2, Path: String;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  FDlgFrm := TscOpenFileDlgForm.Create(Application);
  CreateForm;
  FDlgFrm.OverwritePromt := FOverwritePromt;
  FDlgFrm.CheckFileExists := FCheckFileExists;
  FDlgFrm.DefaultExt := DefaultExt;
  FDlgFrm.SaveMode := FSaveMode;
  FDlgFrm.FileListView.ThumbnailOptions.Enabled := FShowThumbnails and not IsWindowsXP;

  with FDlgFrm do
  try
    if (not FShowThumbnails or IsWindowsXP) and
    ((ListViewStyle = scvsSmallThumbnails) or
    (ListViewStyle = scvsThumbnails) or
    (ListViewStyle = scvsBigThumbnails) or
    (ListViewStyle = scvsVeryBigThumbnails))
    then
      ListViewStyle := scvsList;
    case ListViewStyle of
      scvsList:
      begin
        scGalleryMenu1.FItemIndex := 0;
        FileListView.ViewStyle := vsList;
      end;
      scvsReport:
      begin
        scGalleryMenu1.FItemIndex := 1;
        FileListView.ViewStyle := vsReport;
      end;
      scvsIcon:
      begin
        scGalleryMenu1.FItemIndex := 3;
        FileListView.ViewStyle := vsIcon;
      end;
      scvsSmallIcon:
      begin
        scGalleryMenu1.FItemIndex := 2;
        FileListView.ViewStyle := vsSmallIcon;
      end;
      scvsSmallThumbnails:
      begin
        scGalleryMenu1.FItemIndex := 4;
        FileListView.ThumbnailOptions.SetValues(True, 48, 48);
        FileListView.ViewStyle := vsIcon;
        FileListView.UpdateThumbnails;
      end;
      scvsThumbnails:
      begin
        scGalleryMenu1.FItemIndex := 5;
        FileListView.ThumbnailOptions.SetValues(True, 96, 96);
        FileListView.ViewStyle := vsIcon;
        FileListView.UpdateThumbnails;
      end;
      scvsBigThumbnails:
      begin
        scGalleryMenu1.FItemIndex := 6;
        FileListView.ThumbnailOptions.SetValues(True, 128, 128);
        FileListView.ViewStyle := vsIcon;
        FileListView.UpdateThumbnails;
      end;
      scvsVeryBigThumbnails:
      begin
        scGalleryMenu1.FItemIndex := 7;
        FileListView.ThumbnailOptions.SetValues(True, 256, 256);
        FileListView.ViewStyle := vsIcon;
        FileListView.UpdateThumbnails;
      end;
    end;

    if (FFileName <> '') and (ExtractFilePath(FFileName) <> '')
    then
      begin
        Path := ExtractFilePath(FFileName);
        if DirectoryExists(Path)
        then
          FileListView.Root := Path
        else
          if (FInitialDir <> '') and DirectoryExists(FInitialDir)
          then
            FileListView.Root  := FInitialDir
          else
            FileListView.Root := ExtractFilePath(Application.ExeName);
      end
    else
      begin
        if FInitialDir = ''
        then
          FileListView.Root := ExtractFilePath(Application.ExeName)
        else
          begin
            if DirectoryExists(FInitialDir)
            then
              FileListView.Root  := FInitialDir
            else
              FileListView.Root := ExtractFilePath(Application.ExeName);
          end;
      end;

    FileListView.MultiSelect := MultiSelection;

    if FShowHiddenFiles
    then
      FileListView.ObjectTypes := FileListView.ObjectTypes + [otHidden];

    FilterComboBox.Filter := Self.Filter;
    FilterComboBox.ItemIndex := FFilterIndex - 1;

    FileListView.Mask := FilterComboBox.Mask;

   FW := 0;
   if FDialogClientWidth > 0 then
     FW := FDialogClientWidth;

   FH := 0;
   if FDialogClientHeight > 0 then
     FH := FDialogClientHeight;
 
    if FW > 0 then
      ClientWidth := FW;
    if FH > 0 then
      ClientHeight := FH;

    with FileListView do
    begin
      if Items.Count <> 0 then ItemFocused := Items[0];
    end;

    InitForm;

    FileNameEdit.Text := ExtractFileName(FFileName);

    InitControls;

    if FTitle <> '' then
      Caption := FTitle;

    Result := (ShowModal = mrOk);

    FFilterIndex := FilterComboBox.ItemIndex + 1;

     if FDlgFrm.scStyledForm1.FDPIChanged then
    begin
      FDialogClientWidth := Round(ClientWidth / scStyledForm1.ScaleFactor);
      FDialogClientHeight := Round(ClientHeight / scStyledForm1.ScaleFactor);
    end
    else
    begin
      FDialogClientWidth := ClientWidth;
      FDialogClientHeight := ClientHeight;
    end;

    if FileListView.IsThumbnailView then
    begin
      case FileListView.ThumbnailOptions.Width of
        48: ListViewStyle := scvsSmallThumbnails;
        96: ListViewStyle := scvsThumbnails;
        128: ListViewStyle := scvsBigThumbnails;
        256: ListViewStyle := scvsVeryBigThumbnails;
      end;
    end
    else
      case FileListView.ViewStyle of
        vsIcon:
          ListViewStyle := scvsIcon;
        vsSmallIcon:
          ListViewStyle := scvsSmallIcon;
        vsReport:
          ListViewStyle := scvsReport;
        vsList:
          ListViewStyle := scvsList;
      end;

    if Result
    then
      begin
        Self.FFileName := FDlgFrm.FileName;
        // check ext
        if FSaveMode and (Pos('.*', FilterComboBox.Mask) = 0)
        then
          begin
            if Pos(';', FilterComboBox.Mask) > 0
            then
              Ext1 := ExtractFileExt(Copy(FilterComboBox.Mask, 1,
                       Pos(';', FilterComboBox.Mask)-1))
            else
              Ext1 := ExtractFileExt(FilterComboBox.Mask);
            Ext2 := ExtractFileExt(FFileName);
            if Ext2 = ''
            then
              FFileName := FFileName + Ext1;
          end;
        //
        FileListView.GetSelectedFiles(Self.Files);
        if FSaveMode and (ExtractFileExt(FFileName) = '') and
          (FDefaultExt <> '')
        then
          FFileName := FFileName + '.' + FDefaultExt;
        Change;
      end;
    SaveForm;
  finally
    if Assigned(FOnClose) then FOnClose(Self);
    Free;
    FDlgFrm := nil;
  end;
end;

constructor TscSaveDialog.Create(AOwner: TComponent);
begin
  inherited;
  FSaveMode := True;
end;


end.
