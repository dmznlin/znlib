{ *
  * Twitter_Snowflake https://github.com/twitter-archive/snowflake
  * SnowFlake的结构如下(每部分用-分开):
  * 0 - 0000000000 0000000000 0000000000 0000000000 0 - 00000 - 00000 - 000000000000
  * 1位标识，由于long基本类型在Java中是带符号的，最高位是符号位，正数是0，负数是1，所以id一般是正数，最高位是0
  * 41位时间截(毫秒级)，注意，41位时间截不是存储当前时间的时间截，而是存储时间截的差值（当前时间截 - 开始时间截)
  * 得到的值），这里的的开始时间截，一般是我们的id生成器开始使用的时间，由我们程序来指定的（如下下面程序IdWorker类的startTime属性）。41位的时间截，可以使用69年，年T = (1L << 41) / (1000L * 60 * 60 * 24 * 365) = 69
  * 10位的数据机器位，可以部署在1024个节点，包括5位datacenterId和5位workerId
  * 12位序列，毫秒内的计数，12位的计数顺序号支持每个节点每毫秒(同一机器，同一时间截)产生4096个ID序号
  * 加起来刚好64位，为一个Long型。
  * SnowFlake的优点是，整体上按照时间自增排序，并且整个分布式系统内不会产生ID碰撞(由数据中心ID和机器ID作区分)，并且效率较高，经测试，SnowFlake每秒能够产生409.6万ID左右。
  *
  * 本算法参考官方 Twitter Snowflake 修改而来，同时借鉴了网上Java语言的版本。
  * 作者：全能中间件 64445322 https://www.centmap.cn/server
  * 使用方法：var OrderId := IdGenerator.NextId()，IdGenerator 不用创建也不用释放，而且该方法是线程安全的。
  * }

// 参考美团点评分布式ID生成系统
// https://tech.meituan.com/2017/04/21/mt-leaf.html
// https://github.com/Meituan-Dianping/Leaf/blob/master/leaf-core/src/main/java/com/sankuai/inf/leaf/snowflake/SnowflakeIDGenImpl.java

unit USnowFlake;

interface

uses
  System.SysUtils, System.SyncObjs;

type
  TSnowflakeIdWorker = class(TObject)
  private const
    // 最大可用69年
    MaxYears = 69;
    // 机器id所占的位数
    WorkerIdBits = 5;
    // 数据标识id所占的位数
    DatacenterIdBits = 5;
    // 序列在id中占的位数
    SequenceBits = 12;
    // 机器ID向左移12位
    WorkerIdShift = SequenceBits;
    // 数据标识id向左移17位(12+5)
    DatacenterIdShift = SequenceBits + WorkerIdBits;
    // 时间截向左移22位(5+5+12)
    TimestampLeftShift = SequenceBits + WorkerIdBits + DatacenterIdBits;
	{$WARNINGS OFF}
    // 生成序列的掩码，这里为4095 (0b111111111111=0xfff=4095)
    SequenceMask = -1 xor (-1 shl SequenceBits);
    // 支持的最大机器id
    MaxWorkerId = -1 xor (-1 shl WorkerIdBits);
    // 支持的最大数据标识id，结果是 31
    MaxDatacenterId = -1 xor (-1 shl DatacenterIdBits);
	{$WARNINGS ON}
  private type
    TWorkerID = 0 .. MaxWorkerId;
    TDatacenterId = 0 .. MaxDatacenterId;
  strict private
    FWorkerID: TWorkerID;
    FDatacenterId: TDatacenterId;
    FEpoch: Int64;
    FSequence: Int64;
    FLastTimeStamp: Int64;
    FStartTimeStamp: Int64;
    FUnixTimestamp: Int64;
    FIsHighResolution: Boolean;
    /// <summary>
    /// 阻塞到下一个毫秒，直到获得新的时间戳
    /// </summary>
    /// <param name="ATimestamp ">上次生成ID的时间截</param>
    /// <returns>当前时间戳 </returns>
    function WaitUntilNextTime(ATimestamp: Int64): Int64;
    /// <summary>
    /// 返回以毫秒为单位的当前时间
    /// </summary>
    /// <remarks>
    /// 时间的表达格式为当前计算机时间和1970年1月1号0时0分0秒所差的毫秒数
    /// </remarks>
    function CurrentMilliseconds: Int64; inline;
    function CurrentTimeStamp: Int64; inline;
    function ElapsedMilliseconds: Int64; inline;
  private
    class var FLock: TSpinLock;
    class var FInstance: TSnowflakeIdWorker;
    class function GetInstance: TSnowflakeIdWorker; static;
    class constructor Create;
    class destructor Destroy;
  protected
    function GetEpoch: TDateTime;
    procedure SetEpoch(const Value: TDateTime);
  public
    constructor Create; overload;
    /// <summary>
    /// 获得下一个ID (该方法是线程安全的)
    /// </summary>
    function NextId: Int64;inline;
    /// <summary>
    /// 工作机器ID(0~31)
    /// </summary>
    property WorkerID: TWorkerID read FWorkerID write FWorkerID;
    /// <summary>
    /// 数据中心ID(0~31)
    /// </summary>
    property DatacenterId: TDatacenterId read FDatacenterId write FDatacenterId;
    /// <summary>
    /// 开始时间
    /// </summary>
    property Epoch: TDateTime read GetEpoch write SetEpoch;

    class property Instance: TSnowflakeIdWorker read GetInstance;
  end;

function IdGenerator: TSnowflakeIdWorker;

const
  ERROR_CLOCK_MOVED_BACKWARDS = 'Clock moved backwards. Refusing to generate id for %d milliseconds';
  ERROR_EPOCH_INVALID         = 'Epoch can not be greater than current';

implementation

uses
  System.Math, System.TimeSpan
{$IF defined(MSWINDOWS)}
    , Winapi.Windows
{$ELSEIF defined(MACOS)}
    , Macapi.Mach
{$ELSEIF defined(POSIX)}
    , Posix.Time
{$ENDIF}
    , System.DateUtils;

function IdGenerator: TSnowflakeIdWorker;
begin
  Result := TSnowflakeIdWorker.GetInstance;
end;

{ TSnowflakeIdWorker }

constructor TSnowflakeIdWorker.Create;
{$IF defined(MSWINDOWS)}
var
  Frequency: Int64;
{$ENDIF}
begin
  inherited;
{$IF defined(MSWINDOWS)}
  FIsHighResolution := QueryPerformanceFrequency(Frequency);
{$ELSEIF defined(POSIX)}
  FIsHighResolution := True;
{$ENDIF}
  FSequence := 0;
  FWorkerID := 1;
  FDatacenterId := 1;
  FLastTimeStamp := -1;
  FEpoch := DateTimeToUnix(EncodeDate(2019, 12, 12), True) * MSecsPerSec;
  FUnixTimestamp := DateTimeToUnix(Now, True) * MSecsPerSec;
  FStartTimeStamp := CurrentTimeStamp;
end;

class destructor TSnowflakeIdWorker.Destroy;
begin
  FreeAndNil(FInstance);
end;

class constructor TSnowflakeIdWorker.Create;
begin
  FInstance := nil;
  FLock := TSpinLock.Create(False);
end;

class function TSnowflakeIdWorker.GetInstance: TSnowflakeIdWorker;
begin
  FLock.Enter;
  try
    if FInstance = nil then
      FInstance := TSnowflakeIdWorker.Create;
    Result := FInstance;
  finally
    FLock.Exit;
  end;
end;

function TSnowflakeIdWorker.CurrentTimeStamp: Int64;
{$IF defined(POSIX) and not defined(MACOS)}
var
  res: timespec;
{$ENDIF}
begin
{$IF defined(MSWINDOWS)}
  if FIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := GetTickCount * Int64(TTimeSpan.TicksPerMillisecond);
{$ELSEIF defined(MACOS)}
  Result := Int64(AbsoluteToNanoseconds(mach_absolute_time) div 100);
{$ELSEIF defined(POSIX)}
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 100;
{$ENDIF}
end;

function TSnowflakeIdWorker.ElapsedMilliseconds: Int64;
begin
  Result := (CurrentTimeStamp - FStartTimeStamp) div TTimeSpan.TicksPerMillisecond;
end;

function TSnowflakeIdWorker.GetEpoch: TDateTime;
begin
  Result := UnixToDateTime(FEpoch div MSecsPerSec, True);
end;

function TSnowflakeIdWorker.NextId: Int64;
var
  Offset: Integer;
  Timestamp: Int64;
begin
  FLock.Enter;
  try
    Timestamp := CurrentMilliseconds();

    // 如果当前时间小于上一次ID生成的时间戳，说明系统时钟回退过这个时候应当抛出异常
    if (Timestamp < FLastTimeStamp) then
    begin
      Offset := FLastTimeStamp - Timestamp;
      if Offset <= 5 then
      begin
        // 时间偏差大小小于5ms，则等待两倍时间
        System.SysUtils.Sleep(Offset shr 1);

        Timestamp := CurrentMilliseconds();
        // 还是小于，抛异常并上报
        if Timestamp < FLastTimeStamp then
          raise Exception.CreateFmt(ERROR_CLOCK_MOVED_BACKWARDS, [FLastTimeStamp - Timestamp]);
      end;
    end;

    // 如果是同一时间生成的，则进行毫秒内序列
    if (FLastTimeStamp = Timestamp) then
    begin
      FSequence := (FSequence + 1) and SequenceMask;
      // 毫秒内序列溢出
      if (FSequence = 0) then
        // 阻塞到下一个毫秒,获得新的时间戳
        Timestamp := WaitUntilNextTime(FLastTimeStamp);
    end
    // 时间戳改变，毫秒内序列重置
    else
      FSequence := 0;

    // 上次生成ID的时间截
    FLastTimeStamp := Timestamp;

    // 移位并通过或运算拼到一起组成64位的ID
    Result := ((Timestamp - FEpoch) shl TimestampLeftShift)
      or (DatacenterId shl DatacenterIdShift)
      or (WorkerID shl WorkerIdShift)
      or FSequence;
  finally
    FLock.Exit;
  end;
end;

function TSnowflakeIdWorker.WaitUntilNextTime(ATimestamp: Int64): Int64;
var
  Timestamp: Int64;
begin
  Timestamp := CurrentMilliseconds();
  while Timestamp <= ATimestamp do
    Timestamp := CurrentMilliseconds();

  Result := Timestamp;
end;

procedure TSnowflakeIdWorker.SetEpoch(const Value: TDateTime);
begin
  if Value > Now then
    raise Exception.Create(ERROR_EPOCH_INVALID);

  if YearsBetween(Now, Value) <= MaxYears then
    FEpoch := DateTimeToUnix(Value, True) * MSecsPerSec;
end;

function TSnowflakeIdWorker.CurrentMilliseconds: Int64;
begin
  Result := FUnixTimestamp + ElapsedMilliseconds;
end;

end.