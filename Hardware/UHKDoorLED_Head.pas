{*******************************************************************************
  作者: dmzn@163.com 2019-06-11
  描述: 海康威门岗LED引导屏开发库头文件
*******************************************************************************}
unit UHKDoorLED_Head;

interface

const
  cVTDLL = 'vtLEDProtocol.DLL';

  //显示屏颜色类型定义
  VT_SIGNLE_COLOR             = 1; //单色显示屏
  VT_DOUBLE_COLOR             = 2; //双色显示屏 可显示 红 绿 黄 三色
  VT_FULL_COLOR               = 3; //全彩显示屏 可显示 7 彩色

  //动作方式定义
  VT_ACTION_HOLD              = $01; //静止显示/立即显示/翻页显示
  VT_ACTION_UP                = $1A; //向上移动
  VT_ACTION_DOWN              = $1B; //向下移动
  VT_ACTION_LEFT              = $1C; //向左移动
  VT_ACTION_RIGHT             = $1D; //向右移动
  VT_ACTION_CUP               = $1E; //向上连续移动
  VT_ACTION_CDOWN             = $1F; //向下连续移动
  VT_ACTION_CLEFT             = $20; //向左连续移动
  VT_ACTION_CRIGHT            = $21; //向右连续移动
  VT_ACTION_FLASH             = $29; //闪烁
  
  //显示颜色定义
  VT_COLOR_RED                = $01; //红
  VT_COLOR_GREEN              = $02; //绿
  VT_COLOR_YELLOW             = $04; //黄(红+绿)
  VT_COLOR_BLUE               = $08; //蓝
  VT_COLOR_Cyan               = $10; //紫(红+蓝)
  VT_COLOR_Purple             = $20; //青(绿+蓝)
  VT_COLOR_WHITE              = $40; //白(红+绿+蓝)

  //显示字体定义
  VT_FONT_16                  = $10; //16 点高字体
  VT_FONT_24                  = $18; //24 点高字体
  VT_FONT_32                  = $20; //32 点高字体
  //时间类型定义
  VT_TIME_TYPE_CALENDAR       = $00; //日历时间
  VT_TIME_TYPE_COUNT_DOWN     = $01; //倒计时

  //小时类型定义
  VT_HOUR_TYPE_24H            = $00; //24 小时类型
  VT_HOUR_TYPE_12H            = $01; //12 小时类型

  //时区定义
  VT_TIMEZONE_TYPE_BEIJING    = $00; //北京时间
  VT_TIMEZONE_TYPE_E          = $01; //东区
  VT_TIMEZONE_TYPE_W          = $02; //西区

  //错误状态
  VT_PAR_OK                   = 0; //参数正确
  VT_PAR_DEV_NOT_INIT_ERROR   = -1; //设备参数未设置
  VT_PAR_PROGRAM_ID_ERROR     = -2; //节目 ID 号错误
  VT_PAR_NO_PROGRAM_ERROR     = -3; //节目未初始化
  VT_PAR_AREA_ID_ERROR        = -4; //区域 ID 号错误
  VT_PAR_AREA_ERROR           = -5; //区域坐标设置错误
  VT_PAR_COLOR_ERROR          = -6; //颜色设置错误
  VT_PAR_ACTION_ERROR         = -7; //动作方式设置错误
  VT_PAR_FONT_ERROR           = -8; //字体设置错误
  VT_PAR_SOUND_ONLY_ERROR     = -9; //一个节目中只能含有一条语音
  VT_PAR_DATA_SIZE_ERROR      = -10; //数据长度设置错误
  VT_PAR_MEM_ERROR            = -11; //系统缓存错误
  VT_PAR_FRAME_FLAG_ERROR     = -12; //协议数据帧标志错误
  VT_PAR_FRAME_SIZE_ERROR     = -13; //协议数据帧长度错误
  VT_PAR_CMD_ERROR            = -14; //指令错误
  
type
  vt_int8_t                   = ShortInt;
  vt_int16_t                  = Smallint;
  vt_int32_t                  = Integer;
  vt_uint8_t                  = Byte;
  vt_uint16_t                 = Word;
  vt_uint32_t                 = Longword;

  pvt_int8_t                  = ^ShortInt;
  pvt_int16_t                 = ^Smallint;
  pvt_int32_t                 = ^Integer;
  pvt_uint8_t                 = PChar;
  pvt_uint16_t                = ^Word;
  pvt_uint32_t                = ^Longword;

function vt_ProtocolAnalyze(pData: pvt_uint8_t; nSize: vt_uint32_t;
  pOut: pvt_uint8_t; pnLen: pvt_uint32_t): vt_int32_t; stdcall; external cVTDLL;
function vtInitialize(nWidth,nHeight: vt_uint16_t; nColor: vt_uint8_t;
  nCardType: vt_uint8_t): vt_int32_t; stdcall; external cVTDLL;
function vtUninitialize(): vt_int32_t; stdcall; external cVTDLL;

function vtAddProgram(nProgramID: vt_uint8_t): vt_int32_t; stdcall; external cVTDLL;
function vtGetProgramPack(nDeviceGUID: vt_uint8_t; nType: vt_uint8_t;
  pOut: pvt_uint8_t; pnLen: pvt_uint32_t): vt_int32_t; stdcall; external cVTDLL;
function vtAddTextAreaItem(nProgramID: vt_uint8_t; nAreaID: vt_uint8_t;
  nX,nY,nWidth,nHeight: vt_uint16_t; pText: pvt_uint8_t; nTextSize: vt_uint16_t;
  nTextColor,nStyle,nFontType,nShowSpeed,nStayTime: vt_uint8_t): vt_int32_t;
  stdcall; external cVTDLL;
function vtAddSoundItem(nProgramID: vt_uint8_t; nAreaID: vt_uint8_t;
  SoundPerson,SoundVolume,SoundSpeed: vt_uint8_t;
  pSoundText: pvt_uint8_t; sound_len: vt_uint16_t): vt_int32_t;
  stdcall; external cVTDLL;

//------------------------------------------------------------------------------
const
  cPlayDLL = 'ListenPlayDll.DLL';

function StartSend(): Integer; stdcall; external cPlayDLL;
{
功能：
  启动通讯会话
返回值：
  会话句柄,此值供后续程序调用
}
function EndSend(nHandle: Integer): Integer; stdcall; external cPlayDLL;
{
功能：
  结束通讯会话
返回值：
  1：成功
  2：不成功
}
function SetTransMode(nHandle,nTransMode,nMark,nType,
  nMarkID: Integer): Integer; stdcall; external cPlayDLL;
{
功能：
  设置通讯模式
参数：
  Handle:会话句柄,StartSend返回值
  TransMode:传输模式   1 网口传输 2 串口传输
  mark:默认填为0.如果是rf通讯，填入1
  controlType：型号。2是T系列，3是E，Q系列卡
  Markid：传入屏号值。
返回值：
1：成功
0：不成功
}
function SetNetworkPara(nHandle,nPNum: Integer;
  nIP: PWideChar): Integer; stdcall; external cPlayDLL;
{
功能：
  设置网络参数
参数:
 Handle:会话句柄,StartSend返回值
  pno:屏号
  ip:控制器IP地址
返回值：
  1：成功
  2：不成功
}
function SendScreenPara(nHandle,nColor,nWidth,
  nHeight: Integer): Integer; stdcall; external cPlayDLL;
{
功能：
  设置屏幕参数
参数:
 Handle:会话句柄,StartSend返回值
 nColor:颜色 1,单色;2,双色
 nWidth,nHeight:宽高
返回值：
  1：成功
  2：不成功
}
function AddControl(nHandle,nPNum,nDBColor: Integer): Integer;
  stdcall; external cPlayDLL;
{
功能：添加显示屏
参数：
  Handle:  会话句柄,StartSend返回值
  Pno:屏号
  DBColor：单双色(单色为1 ，双色为2,三基色3)
返回值：
  1、成功
  2、参数错误
}
function AddProgram (nHandle,nJNum,nPlayTime: Integer): Integer;
  stdcall; external cPlayDLL;
{
功能：添加节目
参数：
  Handle:  会话句柄,StartSend返回值
  jno：节目号
  playTime：节目播放时间
返回值：
  1、成功
  2、参数错误
}
function AddNeiMaTxtArea1(const nHandle,nJNum,nQNum,nLeft,nTop,nWidth,
  nHeight: Integer; nText: PWideChar; nShowStyle,nFontName,nFontColor,
  nPlayStyle,nQuitStyle,nPlayspeed,nTimes: Integer): Integer;
  stdcall; external cPlayDLL;
{
功能：添加内码区域
参数：
  handle：	句柄
  Pno:       屏号
  jno：		节目号 (>=1)
  qno：		区域号 (>=1)
  left：		区域左上角顶点x坐标：8的倍数，单位：象素
  top：		区域左上角顶点y坐标
  width：		区域宽度：8的倍数，单位：象素
  height：		区域高度
  Showtext   发送的内容（如"欢迎光临"）
  ShowStyle  点阵大小（取值为16，32。16表示16点阵，32表示32点阵，其他取值无效。
  Fontname   字体索引（取值为0表示宋体，其他为无效值)
  Fontcolor   字体颜色(取值为1，2，3。1--红色，2--绿色，3--黄色)
  PlayStyle   播放特技
  QuitStyle   退场特技---默认值255
  PlaySpeed  播放速度（取值1--255，表示等级，数值越高，速度越慢）
  Times      停留时间（取值1-255）
返回值：
  5.成功
  6.失败
}
function AddLnTxtString(nHandle,nJNum,nQNum,nLeft,nTop,nWidth,nHeight: Integer;
  nText,nFontName: PWideChar; nFontSize,nFontColor,nBold,nItalic,nUnderline,
  nPlayStyle,nPlaySpeed,nTimes: Integer): Integer; stdcall; external cPlayDLL;
{
功能：
  添加单行文本（使用字符串）
参数：
  jno：		节目号 (>=1)
  qno：		区域号 (>=1)

  left：		区域左上角顶点x坐标：8的倍数，单位：象素
  top：		区域左上角顶点y坐标
  width：		区域宽度：8的倍数，单位：象素
  height：		区域高度

  Fontname    字体名称
  Fontsize     字体大小
  Fontcolor    字体颜色（255－－红色，65280－－黄色，65535－－绿色);
  Bold        是否粗体
  Italic        是否斜体
  Underline    是否下划线

  PlayStyle：  		显示特技（支持左移、右移、上移、下移）
  Playspeed：		显示速度
  Times           保留参数（暂未使用）
返回值：
  1、成功
  2、参数错误
}
function SendControl(nHandle,nSendType: Integer;nHWnd: THandle): Integer;
  stdcall; external cPlayDLL;
{
功能：发送数据
参数：
  SendType:发送模式1为普通 2为SD卡发送
  Hwnd:窗口句柄 ,一般取0即可
返回值：
  0：原因：1,没有添加节目2.若为网络发送，则端口被占用 3.若为串口发送则串口被占用或不存在
  1：发送成功
  2：通讯失败
  3：发送过程中出错
}

implementation

end.
