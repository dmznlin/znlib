*.git path
https://github.com/norgepaul/DelphiKafkaClient.git/trunk

*.修正内容:
*.官方开发库地址 
  https://github.com/edenhill/librdkafka
  Delphi使用的头文件转自官方库,比DelphiKafkaClient版本高.

  Delphi使用的DLL文件,来自 
  https://www.nuget.org/packages/librdkafka.redist

*.使用方法:
1.从github获取完整版本的KafkaClient并添加到Library Path中.
2.将本路径添加到刚才加入的KafkaClient之前,保证优先被扫描到.