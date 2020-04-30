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

unit scBitmapEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, PngImage;

type
  TscBitmapEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    PaintBox: TPaintBox;
    OD: TOpenDialog;
    SD: TSaveDialog;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FBitmap: TBitmap;
    FPngImage: TPngImage;
    PngMode: Boolean;
  end;

  procedure ExecutePngToBitmapEditor(ABitmap: TBitmap; APremultiplied: Boolean);
  procedure ExecutePngEditor(APngImage: TPngImage);

implementation
  Uses scDrawUtils;

{$R *.dfm}

procedure ExecutePngEditor(APngImage: TPngImage);
var
  F: TscBitmapEditorForm;
begin
  F := TscBitmapEditorForm.Create(Application);
  F.PngMode := True;
  try
    F.Position := poScreenCenter;
    F.FPngImage.Assign(APngImage);
    if F.ShowModal = mrOk then
      APngImage.Assign(F.FPngImage);
  finally
    F.Free;
  end;
end;

procedure ExecutePngToBitmapEditor(ABitmap: TBitmap; APremultiplied: Boolean);
var
  F: TscBitmapEditorForm;
begin
  F := TscBitmapEditorForm.Create(Application);
  try
    F.Position := poScreenCenter;
    if not APremultiplied then
      F.FBitmap.AlphaFormat := afIgnored;
    F.FBitmap.Assign(ABitmap);
    if not APremultiplied then
      F.FBitmap.AlphaFormat := afPremultiplied;
    if F.ShowModal = mrOk then
      ABitmap.Assign(F.FBitmap);
  finally
    F.Free;
  end;
end;

procedure TscBitmapEditorForm.Button1Click(Sender: TObject);
begin
  if OD.Execute then
  begin
    FPngImage := TPngImage.Create;
    FPngImage.LoadFromFile(OD.FileName);
    if not PngMode then
    begin
      if FPngImage.TransparencyMode = ptmPartial then
      begin
        FBitmap.AlphaFormat := afIgnored;
        FBitmap.Assign(FPngImage);
        FBitmap.AlphaFormat := afPremultiplied;
      end
      else
        FBitmap.Assign(FPngImage);
    end;
    PaintBox.Repaint;
  end;
end;

procedure TscBitmapEditorForm.Button2Click(Sender: TObject);
var
  P: TPngImage;
begin
  if PngMode then
  begin
    if not FPngImage.Empty then
      if SD.Execute then
        FPngImage.SaveToFile(SD.FileName);
  end
  else
  if not FBitmap.Empty then
    if SD.Execute then
    begin
      P := TPngImage.Create;
      try
        CreatePngFromBitmap(FBitmap, P);
        P.SaveToFile(SD.FileName);
      finally
        P.Free;
      end;
    end;
end;

procedure TscBitmapEditorForm.Button3Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TscBitmapEditorForm.Button4Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TscBitmapEditorForm.Button5Click(Sender: TObject);
begin
  if PngMode then
  begin
    FPngImage.Assign(nil);
    PaintBox.Repaint;
  end
  else
  begin
    FBitmap.Assign(nil);
    PaintBox.Repaint;
  end;
end;

procedure TscBitmapEditorForm.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  FPngImage := TPngImage.Create;
end;

procedure TscBitmapEditorForm.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
  FPngImage.Free;
end;

procedure TscBitmapEditorForm.PaintBoxPaint(Sender: TObject);
var
  X, Y: Integer;
begin
  if PngMode then
  begin
    if FPngImage.Empty then Exit;
    X := PaintBox.Width div 2 - FPngImage.Width div 2;
    Y := PaintBox.Height div 2 - FPngImage.Height div 2;
    if (X < 0) or (Y < 0) then
      PaintBox.Canvas.StretchDraw(Rect(0, 0, PaintBox.Width, PaintBox.Height),
       FPngImage)
    else
      PaintBox.Canvas.Draw(X, Y, FPngImage);
  end
  else
  begin
    if FBitmap.Empty then Exit;
    X := PaintBox.Width div 2 - FBitmap.Width div 2;
    Y := PaintBox.Height div 2 - FBitmap.Height div 2;
    if (X < 0) or (Y < 0)  then
    begin
      if FBitmap.PixelFormat = pf32bit then
        Bitmap_DrawAlpha(FBitmap, PaintBox.Canvas, Rect(0, 0, FBitmap.Width,
          FBitmap.Height), Rect(0, 0, PaintBox.Width, PaintBox.Height), 255)
      else
        PaintBox.Canvas.StretchDraw(Rect(0, 0, PaintBox.Width, PaintBox.Height),
          FBitmap)
    end
    else
    begin
      if FBitmap.PixelFormat = pf32bit then
        Bitmap_DrawAlpha_XY(FBitmap, PaintBox.Canvas, X, Y, 255)
      else
        PaintBox.Canvas.Draw(X, Y, FBitmap);
    end;
  end;
end;

end.
