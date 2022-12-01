# Client-Server-TestTask
Client-Server application, Sockets, Threads


Серверная часть - TestTaskServer.dpr<br />
в виде простого консольного приложения<br />
<br />
Клиентская часть - ClientProject.dpr, ClientUnit.pas, ClientUnit.dfm<br />
Быстро накидал простую VCL-ку<br />
Есть кнопка для подключения/отключения к сервеному приложению<br />
Вторая кнопка - отправка пакета, впрочем ее нажимает таймер,<br />
пока соединение активно<br />
<br />
На сервере прикрутил для теста SQLite3, но и консольный вывод тоже дает понять, <br />
что всё работает корректно<br />
<br />
я оставил закомментированными строки<br />
  //Srv.OnClientConnect:=ClientConnect;<br />
  //Srv.OnClientDisconnect:=ClientDisconnect;<br />
  //Srv.OnClientRead:=ClientRead;<br />
это был мой первый вариант реализации, в однопоточном режиме<br />
он тоже работал корректно, но раз мы про потоки, сообразил,<br />
переделал на stThreadBlocking<br />
<br />
//IBQuery1.SQL.Clear;<br />
//IBQuery1.SQL.Add('INSERT INTO TestTaskTable(client_id,data) VALUES ('+IntToStr(ClientSocket.SocketHandle)',"'+incomingData+'")');<br />
//IBQuery1.ExecSQL;<br />
эту часть оставил в закомментированном виде, как пример записи в БД через стандартные средства<br />
<br />
WriteLn и запись в БД внутри критической секции во избежание состояния гонки<br />
