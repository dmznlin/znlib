{*******************************************************************************
                                 Falcon Sistemas

                           www.falconsistemas.com.br
                         suporte@falconsistemas.com.br
                 Written by Marlon Nardi - ALL RIGHTS RESERVED.

                   https://github.com/craftpip/jquery-confirm

*******************************************************************************}

{$DEFINE FolderUni} //Adicione a pasta "falcon" dentro da pasta do uniGui (uni-XXX)
{$IF CompilerVersion >= 24.0} // XE3 ou superior
  {$LEGACYIFEND ON}
{$IFEND}

unit UniFSConfirm;

interface

uses
  Classes, TypInfo, SysUtils, uniGUIApplication, uniGUITypes, uniGUIClasses, UniFSCommon;

const
  FSAbout = 'www.falconsistemas.com.br';
  PackageVersion = '1.0.1.15';

type
  TTypeConfirm = (Confirm, Alert, Dialog, Prompt);
  TTypeColor = (blue, green, orange, purple, dark_, red);
  TTheme = (light, dark, modern, supervan, material, bootstrap);
  TConfirmButton = (Other, Yes, No, Ok);
  TTypePrompt = (text, password);

  TButtonCallBack = reference to procedure(ConfirmButton: TConfirmButton);
  TPromptCallBack = reference to procedure(ConfirmButton: TConfirmButton; Result: string);

  TUniFSScreenMask = class(TPersistent)
  private
    FEnabled: Boolean;
    FText: string;
  public
    constructor Create;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Text: string read FText write FText;
  end;

  TUniFSPrompt = class(TPersistent)
  private
    FTypePrompt: TTypePrompt;
    FRequiredField: Boolean;
    FTextRequiredField: string;
  public
    constructor Create;
  published
    property TypePrompt: TTypePrompt read FTypePrompt write FTypePrompt;
    property RequiredField: Boolean read FRequiredField write FRequiredField;
    property TextRequiredField: string read FTextRequiredField write FTextRequiredField;

  end;

  {$IF CompilerVersion >= 23.0} // XE2 ou superior
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TUniFSConfirm = class(TUniComponent)
  protected
    function GetVersion: string;
    function GetAbout: string;

    function GetStrTypeConfirm(TypeConfirm: TTypeConfirm): string;
    function GetStrTypeColor(TypeColor: TTypeColor): string;
    function GetStrTheme(Theme: TTheme): string;
    function GetStrTypePrompt(TypePrompt: TTypePrompt): string;

    procedure WebCreate; override;
    procedure DOHandleEvent(EventName: string; Params: TUniStrings); override;
    procedure LoadCompleted; override;

    procedure RemoveInvalidChar(var InputText: string);

    function BoolToStr(const value: boolean): string; {Versions Old Delphi}
    function BuildJS(TypeConfirm: TTypeConfirm): string;

    procedure ExecJS(JS: string);
  private
    FTitle: string;
    FContent: string;
    FTheme: TTheme;
    FTypeColor: TTypeColor;
    FTypeAnimated: Boolean;
    FDraggable: Boolean;
    FEscapeKey: Boolean;
    FCloseIcon: Boolean;
    FIcon: string;
    FRTL: Boolean;
    FButtonTextConfirm: string;
    FButtonTextCancel: string;
    FScreenMask: TUniFSScreenMask;
    FPromptType: TUniFSPrompt;
    FboxWidth: string;
    FMsgPrompt: string;

    FButtonCallBack: TButtonCallBack;
    FPromptCallBack: TPromptCallBack;
  published
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
    property Theme: TTheme read FTheme write FTheme;
    property TypeColor: TTypeColor read FTypeColor write FTypeColor;
    property TypeAnimated: Boolean read FTypeAnimated write FTypeAnimated;
    property Draggable: Boolean read FDraggable write FDraggable;
    property EscapeKey: Boolean read FEscapeKey write FEscapeKey;
    property CloseIcon: Boolean read FCloseIcon write FCloseIcon;
    property Icon: string read FIcon write FIcon;
    property RTL: Boolean read FRTL write FRTL;
    property boxWidth: string read FboxWidth write FboxWidth;

    property ButtonTextConfirm: string read FButtonTextConfirm write FButtonTextConfirm;
    property ButtonTextCancel: string read FButtonTextCancel write FButtonTextCancel;

    property ScreenMask: TUniFSScreenMask read FScreenMask write FScreenMask;
    property PromptType: TUniFSPrompt read FPromptType write FPromptType;

    property About : string read GetAbout;
    property Version : string read GetVersion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure RemovePreLoader;

    procedure Alert(const Title, Content: string); overload;
    procedure Alert(const Title, Content, Icon: string; Color: TTypeColor; Theme: TTheme); overload;
    procedure Alert(const Title, Content, Icon: string; Color: TTypeColor; Theme: TTheme; BC: TButtonCallBack); overload;
    procedure Prompt(const Title, Msg: string; PC: TPromptCallBack); overload;
    procedure Prompt(const Title, Msg, Icon: string; Color: TTypeColor; Theme: TTheme; PC: TPromptCallBack); overload;
    procedure Question(const Title, Content: string; BC: TButtonCallBack); overload;
    procedure Question(const Title, Content, Icon: string; BC: TButtonCallBack); overload;
    procedure Question(const Title, Content, Icon: string; Color: TTypeColor; Theme: TTheme; BC: TButtonCallBack); overload;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Falcon', [TUniFSConfirm]);
end;

{ TUniFSConfirm }

procedure TUniFSConfirm.Alert(const Title, Content: string);
begin
  FTitle := Title;
  FContent := Content;
  ExecJS('$.alert({'+BuildJS(TTypeConfirm.Alert)+'});');
end;

procedure TUniFSConfirm.Alert(const Title, Content, Icon: string;
  Color: TTypeColor; Theme: TTheme);
begin
  FIcon := Icon;
  FTypeColor := Color;
  FTheme := Theme;
  Alert(Title, Content);
end;

procedure TUniFSConfirm.Alert(const Title, Content, Icon: string;
  Color: TTypeColor; Theme: TTheme; BC: TButtonCallBack);
begin
  FButtonCallBack := BC;
  Alert(Title, Content, Icon, Color, Theme);
end;

function TUniFSConfirm.BoolToStr(const value: boolean): string;
begin
  if value then Result := 'true' else Result := 'false';
end;

function TUniFSConfirm.BuildJS(TypeConfirm: TTypeConfirm): string;
var
  StrBuilder: TStringBuilder;
begin
  RemoveInvalidChar(FTitle);
  RemoveInvalidChar(FContent);
  StrBuilder := TStringBuilder.Create;
  try
    with StrBuilder do
    begin
      Append('title: '''+FTitle+''',');
      Append('content: '''+FContent+''', ');
      Append('icon: '''+FIcon+''',  ');

      Append('type: '''+GetStrTypeColor(FTypeColor)+''', ');
      Append('theme: '''+GetStrTheme(FTheme)+''', ');
      Append('closeIcon: '+BoolToStr(FCloseIcon)+', ');
      Append('typeAnimated: '+BoolToStr(FTypeAnimated)+', ');
      Append('draggable: '+BoolToStr(FDraggable)+', ');
      Append('escapeKey: '+BoolToStr(FEscapeKey)+', ');
      Append('rtl: '+BoolToStr(FRTL)+', ');
      Append('useBootstrap: false, ');
      Append('boxWidth: '''+FboxWidth+''', ');

      if TypeConfirm = TTypeConfirm.Confirm then
      begin
        Append('buttons: {');
        Append('  confirma: { ');
        Append('     text: '''+FButtonTextConfirm+''', ');
        Append('     action: function() {');
        if ScreenMask.Enabled then
          Append('      $(''body'').preloader({text: '''+ScreenMask.Text+'''});  ');
        Append('        ajaxRequest('+Self.JSName+', "Confirm", ["Button="+"Yes"]); ');
        Append('     } ');
        Append('  },');

        Append('  cancela: { ');
        Append('     text: '''+FButtonTextCancel+''', ');
        Append('     action: function() {');
        if ScreenMask.Enabled then
          Append('      $(''body'').preloader({text: '''+ScreenMask.Text+'''});  ');
        Append('        ajaxRequest('+Self.JSName+', "Confirm", ["Button="+"No"]); ');
        Append('     } ');
        Append('  }');
        Append('}');
      end;

      if TypeConfirm = TTypeConfirm.Prompt then
      begin
        Append('content: ');
        Append('  ''<form action="" class="formName">'' + ');
        Append('  ''<div class="fs-form-group">'' + ');
        Append('  ''<label class="fs-label">'+FMsgPrompt+'</label>'' + ');
        if FPromptType.RequiredField then
          Append('  ''<input type="'+GetStrTypePrompt(FPromptType.TypePrompt)+'" class="name fs-form-control" required />'' + ')
        else
          Append('  ''<input type="'+GetStrTypePrompt(FPromptType.TypePrompt)+'" class="name fs-form-control" />'' + ');
        Append('  ''</div>'' + ');
        Append('  ''</form>'', ');

        Append('buttons: {');
        Append('  formSubmit: { ');
        //Append('     text: ''OK'', ');
        Append('     text: '''+FButtonTextConfirm+''', ');
        Append('     action: function() {');
        if FPromptType.RequiredField then
        begin
          Append('if (this.$content.find(''.name'').val() === "") { ');
          Append('   $.alert({title:'''+FPromptType.TextRequiredField+''', content:'' '',  useBootstrap: false, boxWidth: '''+FboxWidth+'''}); ');
          Append('   return false; ');
          Append('}');
        end;
        if ScreenMask.Enabled then
          Append('      $(''body'').preloader({text: '''+ScreenMask.Text+'''});  ');
        Append('        ajaxRequest('+Self.JSName+', "Prompt", ["Button="+"Yes","result="+this.$content.find(''.name'').val()]); ');
        Append('     } ');
        Append('  },');

        Append('  cancel: { ');
        Append('     text: '''+FButtonTextCancel+''', ');
        Append('     action: function() {');
        if ScreenMask.Enabled then
          Append('      $(''body'').preloader({text: '''+ScreenMask.Text+'''});  ');
        Append('        ajaxRequest('+Self.JSName+', "Prompt", ["Button="+"No"]); ');
        Append('     } ');
        Append('  }');
        Append('},');

        Append('onContentReady: function () { ');
        Append('    var jc = this; ');
        Append('    this.$content.find(''form'').on(''submit'', function (e) { ');
        Append('        e.preventDefault(); ');
        Append('        jc.$$formSubmit.trigger(''click''); ');
        Append('    }); ');
        Append('} ');
      end;

      if TypeConfirm = TTypeConfirm.Alert then
      begin
        Append('buttons: {');
        Append('  OK: { ');
        Append('     text: '''+FButtonTextConfirm+''', ');

        if Assigned(FButtonCallBack) then
        begin
          Append('     action: function() {');
          if ScreenMask.Enabled then
            Append('      $(''body'').preloader({text: '''+ScreenMask.Text+'''});  ');
          Append('        ajaxRequest('+Self.JSName+', "Alert", ["Button="+"Ok"]); ');
          Append('     } ');
        end;

        Append('  }');
        Append('}');
      end;
    end;

    Result := StrBuilder.ToString;
  finally
    FreeAndNil(StrBuilder);
  end;

end;

procedure TUniFSConfirm.Clear;
begin
  FTitle := EmptyStr;
  FContent := EmptyStr;
  FIcon := EmptyStr;
  FCloseIcon := False;
end;

constructor TUniFSConfirm.Create(AOwner: TComponent);
begin
  inherited;
  FScreenMask := TUniFSScreenMask.Create;
  FPromptType := TUniFSPrompt.Create;

  FTheme := TTheme.modern;
  FButtonTextConfirm := 'Confirma';
  FButtonTextCancel := 'Cancela';
  FboxWidth := '420px';
  FIcon := 'fa fa-smile-o';
end;

destructor TUniFSConfirm.Destroy;
begin
  inherited;
  FreeAndNil(FScreenMask);
  FreeAndNil(FPromptType);
end;

procedure TUniFSConfirm.DOHandleEvent(EventName: string; Params: TUniStrings);
begin
  inherited;
  if EventName = 'Confirm' then
  begin
    if Assigned(FButtonCallBack) then
    begin
      if Params.Values['Button'] = 'Yes' then
        FButtonCallBack(TConfirmButton.Yes);
      if Params.Values['Button'] = 'No' then
        FButtonCallBack(TConfirmButton.No);
      if Params.Values['Button'] = 'Ok' then
        FButtonCallBack(TConfirmButton.Ok);
    end;
  end;
  if EventName = 'Prompt' then
  begin
    if Assigned(FPromptCallBack) then
    begin
      if Params.Values['Button'] = 'Yes' then
        FPromptCallBack(TConfirmButton.Yes, Params.Values['result']);
      if Params.Values['Button'] = 'No' then
        FPromptCallBack(TConfirmButton.No, EmptyStr);
      if Params.Values['Button'] = 'Ok' then
        FButtonCallBack(TConfirmButton.Ok);
    end;
  end;
  if EventName = 'Alert' then
  begin
    if Assigned(FButtonCallBack) then
    begin
      if Params.Values['Button'] = 'Ok' then
        FButtonCallBack(TConfirmButton.Ok);

      FButtonCallBack := nil;
    end;
  end;

  if (EventName <> EmptyStr) and (Params.Values['Button'] <> EmptyStr) then
    ExecJS('$(''body'').preloader(''remove'');');
end;

procedure TUniFSConfirm.ExecJS(JS: string);
begin
  UniSession.AddJS(JS);
end;

function TUniFSConfirm.GetAbout: string;
begin
  Result := FSAbout;
end;

function TUniFSConfirm.GetStrTypePrompt(TypePrompt: TTypePrompt): string;
begin
  Result := GetEnumName(TypeInfo(TTypePrompt), Integer(TypePrompt));
end;

function TUniFSConfirm.GetStrTheme(Theme: TTheme): string;
begin
  Result := GetEnumName(TypeInfo(TTheme), Integer(Theme));
end;

function TUniFSConfirm.GetStrTypeColor(TypeColor: TTypeColor): string;
begin
  Result := GetEnumName(TypeInfo(TTypeColor), Integer(TypeColor));
  Result := StringReplace(Result, '_','',[rfReplaceAll]);
end;

function TUniFSConfirm.GetStrTypeConfirm(TypeConfirm: TTypeConfirm): string;
begin
  Result := GetEnumName(TypeInfo(TTypeConfirm), Integer(TypeConfirm));
end;

function TUniFSConfirm.GetVersion: string;
begin
  Result := PackageVersion;
end;

procedure TUniFSConfirm.LoadCompleted;
begin
  inherited;
end;

procedure TUniFSConfirm.Prompt(const Title, Msg, Icon: string;
  Color: TTypeColor; Theme: TTheme; PC: TPromptCallBack);
begin
  FIcon := Icon;
  FTypeColor := Color;
  FTheme := Theme;
  Prompt(Title, Msg, PC);
end;

procedure TUniFSConfirm.Prompt(const Title, Msg: string; PC: TPromptCallBack);
begin
  FPromptCallBack := PC;
  FTitle := Title;
  FMsgPrompt := Msg;
  FContent := EmptyStr;
  Self.ExecJS('$.confirm({'+BuildJS(TTypeConfirm.Prompt)+'});');
end;

procedure TUniFSConfirm.Question(const Title, Content, Icon: string;
  Color: TTypeColor; Theme: TTheme; BC: TButtonCallBack);
begin
  FIcon := Icon;
  FTypeColor := Color;
  FTheme := Theme;
  Question(Title, Content, BC);
end;

procedure TUniFSConfirm.RemoveInvalidChar(var InputText: string);
begin
  InputText := StringReplace(InputText,#$D,'',[rfReplaceAll]);
  InputText := StringReplace(InputText,#$A,'',[rfReplaceAll]);
  InputText := StringReplace(InputText,#$D#$A,'',[rfReplaceAll]);
  InputText := StringReplace(InputText,#13,'',[rfReplaceAll]);
  InputText := StringReplace(InputText,#13#10,'',[rfReplaceAll]);
  InputText := StringReplace(InputText,'"','',[rfReplaceAll]);
  InputText := StringReplace(InputText,'''','',[rfReplaceAll]);
end;

procedure TUniFSConfirm.RemovePreLoader;
begin
  ExecJS('$(''body'').preloader(''remove'');');
end;

procedure TUniFSConfirm.Question(const Title, Content, Icon: string;
  BC: TButtonCallBack);
begin
  FIcon := Icon;
  Question(Title, Content, BC);
end;

procedure TUniFSConfirm.Question(const Title, Content: string; BC: TButtonCallBack);
begin
  FButtonCallBack := BC;
  FTitle := Title;
  FContent := Content;

  Self.ExecJS('$.confirm({'+BuildJS(TTypeConfirm.Confirm)+'});');
end;

procedure TUniFSConfirm.WebCreate;
begin
  inherited;
  JSComponent := TJSObject.JSCreate('Object');
end;

{ TUniFSScreenMask }

constructor TUniFSScreenMask.Create;
begin
  FText := 'Processing';
end;

{ TUniFSPrompt }

constructor TUniFSPrompt.Create;
begin
  FTextRequiredField := 'Field riquired';
end;

initialization
  {$IFDEF FolderUni}
  UniAddCSSLibrary(CDN+'falcon/css/jquery-confirm.min.css', CDNENABLED, [upoFolderUni, upoPlatformBoth]);
  UniAddCSSLibrary(CDN+'falcon/css/preloader.css?v=1', CDNENABLED, [upoFolderUni, upoPlatformBoth]);
  UniAddJSLibrary(CDN+'falcon/js/jquery-confirm.min.js', CDNENABLED, [upoFolderUni, upoPlatformBoth]);
  UniAddJSLibrary(CDN+'falcon/js/jquery.preloader.min.js', CDNENABLED, [upoFolderUni, upoPlatformBoth]);
  {$ENDIF}
end.
