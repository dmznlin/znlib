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

unit scFontViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, scGPFontControls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Vcl.Themes;

type
  TFontViewerForm = class(TForm)
    ListView1: TListView;
    ImageList1: TImageList;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FInitIndex: Integer;
    constructor CreateEx(AOwner: TComponent; AIndex: Integer);
  end;

  procedure ExecuteGPCharEditor(var AIndex: Integer);

implementation
   Uses scDrawUtils;

{$R *.dfm}

procedure ExecuteGPCharEditor(var AIndex: Integer);
var
  F: TFontViewerForm;
begin
  F := TFontViewerForm.CreateEx(Application, AIndex);
  try
    if (F.ShowModal = mrOk) and (F.ListView1.Selected <> nil) then
      AIndex := F.ListView1.Selected.Index;
  finally
    F.Free;
  end;
end;

constructor TFontViewerForm.CreateEx(AOwner: TComponent; AIndex: Integer);
var
  I: Integer;
begin
  inherited Create(AOwner);
  if AIndex < 0 then
    AIndex := 0;
  if AIndex > 734 then
    AIndex := 734;
  FInitIndex := AIndex;
  ListView1.Items.BeginUpdate;
  for I := 0 to 734 do
    ListView1.Items.Add;
  ListView1.Items.EndUpdate;
end;

procedure TFontViewerForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFontViewerForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFontViewerForm.FormShow(Sender: TObject);
var
  Item: TListItem;
  R: TRect;
begin
  Item := ListView1.Items[FInitIndex];
  Item.Selected := True;
  R := Item.DisplayRect(drBounds);
  if not R.IntersectsWith(ListView1.ClientRect) then
    ListView1.Scroll(0, R.Top);
end;

procedure TFontViewerForm.ListView1AdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  R: TRect;
  C: TColor;
begin
  if Stage <> cdPostPaint then
    Exit;

  R := Item.DisplayRect(drBounds);
  if not R.IntersectsWith(ListView1.ClientRect) then
    Exit;

  C := GetStyleColor(clWindowText);

  if (not StyleServices.Enabled) or TStyleManager.IsCustomStyleActive and Item.Selected then
  begin
    if TStyleManager.IsCustomStyleActive then
    begin
      Sender.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighLight);
      C := StyleServices.GetSystemColor(clHighLightText)
    end
    else
    begin
      C := clHighLightText;
      Sender.Canvas.Brush.Color := clHighLight;
    end;
    Sender.Canvas.FillRect(R);
    DefaultDraw := False;
  end;

  SC_FontAwesome.Draw(ListView1.Canvas, C, 255, R, 32, Item.Index);

end;

end.
