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

unit scOpenPictureDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scControls, Vcl.Menus, Vcl.StdCtrls,
  Jpeg, PngImage, scExtControls, Vcl.ExtCtrls,
  Vcl.ComCtrls, scShellControls, Vcl.ImgList, Vcl.Mask, scStyledForm, scOpenFileDialog,
  scImageCollection;

type
  TscOpenPictureDialog = class(TscOpenDialog)
  protected
    Image: TscImage;
    FImageWidth: Integer;
    FImageCollection: TscImageCollection;
    procedure CreateForm; override;
    procedure InitForm; override;
    procedure SaveForm; override;
    procedure OnFLVChange(Sender: TObject);
    procedure OnImageDblClick(Sender: TObject);
    procedure OnChangeScale(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

   TscSavePictureDialog = class(TscOpenPictureDialog)
     constructor Create(AOwner: TComponent); override;
   end;

implementation
   Uses Winapi.ShlObj, WinApi.ShellApi, scDrawUtils, System.TypInfo, System.UITypes;

{$R scOpenPictureDialog.res}

constructor TscOpenPictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  FImageCollection := TscImageCollection.Create(Self);
  FImageWidth := 250;
  FFilter := 'All (*.bmp;*.ico;*.emf;*.wmf;*.png)|*.bmp;*.ico;*.emf;*.wmf;*.png|' +
   'Bitmaps (*.bmp)|*.bmp|PNG (*.png)|*.png|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';
end;

destructor TscOpenPictureDialog.Destroy;
begin
  FImageCollection.Free;
  inherited;
end;

procedure TscOpenPictureDialog.CreateForm;
var
  Splitter: TSplitter;
  Item: TscImageCollectionItem;
begin
  if FDlgFrm = nil then Exit;

  FDlgFrm.ToolBarPanel.OnChangeScale := OnChangeScale;

  if FImageCollection.Images.Count = 0 then
  begin
    Item := FImageCollection.Images.Add;
    Item.DrawStyle := idsCenter;
    OnChangeScale(Self);
  end
  else
  begin
    OnChangeScale(Self);
  end;

  Image := TscImage.Create(FDlgFrm);
  with Image do
  begin
    Image.Proportional := True;
    Image.Wallpapers := FImageCollection;
    Image.WallpaperIndex := 0;
    Image.Center := True;
    Image.Align := alRight;
    Image.Width := FImageWidth;
    Image.BorderStyle := scpbsFlat;
    Image.StyleKind := scpsFormBackground;
    Image.OnDblClick := OnImageDblClick;
    Parent := FDlgFrm.ClientPanel;
  end;

  Splitter := TSplitter.Create(FDlgFrm);
  Splitter.Align := alRight;
  Splitter.Width := 5;
  Splitter.Align := alRight;
  Splitter.ResizeStyle := rsUpdate;
  Splitter.Parent := FDlgFrm.ClientPanel;
  Splitter.AutoSnap := True;

  FDlgFrm.FOnFolderChange := OnFLVChange;
end;

procedure TscOpenPictureDialog.OnChangeScale(Sender: TObject);
begin
  if FDlgFrm.ToolBarPanel.ScaleFactor >= 2 then
    FImageCollection.Images.Items[0].LoadPngFromResourceName(HInstance, 'SC_IMAGEPREVIEW200')
  else
  if FDlgFrm.ToolBarPanel.ScaleFactor >= 1.5 then
    FImageCollection.Images.Items[0].LoadPngFromResourceName(HInstance, 'SC_IMAGEPREVIEW150')
  else
    FImageCollection.Images.Items[0].LoadPngFromResourceName(HInstance, 'SC_IMAGEPREVIEW');
  FImageCollection.Images.Items[0].CheckBitmapOptions;
  FImageCollection.Images.Items[0].Color := GetCheckBoxTextColor(scsDisabled);
end;

procedure TscOpenPictureDialog.OnFLVChange(Sender: TObject);
begin
  if FDlgFrm.FileListView.GetSelectedFile <> ''
  then
    begin
      Image.WallpaperIndex := -1;
      Image.Picture.LoadFromFile(FDlgFrm.FileListView.GetSelectedFile);
    end
  else
    begin
      if (Image.Picture.Graphic <> nil) and not Image.Picture.Graphic.Empty then
        Image.Picture.Assign(nil);
      Image.WallpaperIndex := 0;
    end;
end;

procedure TscOpenPictureDialog.OnImageDblClick(Sender: TObject);
begin
  if (Image.Picture.Graphic <> nil) and not Image.Picture.Graphic.Empty then
    FDlgFrm.OKButtonClick(FDlgFrm);
end;

procedure TscOpenPictureDialog.InitForm;
begin
  inherited;
  if DialogClientWidth = 0 then
    FDlgFrm.ClientWidth := FDlgFrm.ClientWidth + 165;
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    Image.WallpaperIndex := -1;
    Image.Picture.LoadFromFile(FFileName);
  end
  else
    Image.WallpaperIndex := 0;
end;

procedure TscOpenPictureDialog.SaveForm;
begin
  FImageWidth := Round(Image.Width / Image.ScaleFactor);
end;

constructor TscSavePictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  FSaveMode := True;
end;

end.
