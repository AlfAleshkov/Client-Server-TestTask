# Client-Server-TestTask
Client-Server application, Sockets, Threads


Серверная часть - TestTaskServer.dpr
в виде простого консольного приложения

Клиентская часть - ClientProject.dpr, ClientUnit.pas, ClientUnit.dfm
Быстро накидал простую VCL-ку
Есть кнопка для подключения/отключения к сервеному приложению
Вторая кнопка - отправка пакета, впрочем ее нажимает таймер,
пока соединение активно

На сервере прикрутил для теста SQLite3, но и консольный вывод тоже дает понять, 
что всё работает корректно

я оставил закомментированными строки
  //Srv.OnClientConnect:=ClientConnect;
  //Srv.OnClientDisconnect:=ClientDisconnect;
  //Srv.OnClientRead:=ClientRead;
это был мой первый вариант реализации, в однопоточном режиме
он тоже работал корректно, но раз мы про потоки, сообразил,
переделал на stThreadBlocking

//IBQuery1.SQL.Clear;
//IBQuery1.SQL.Add('INSERT INTO TestTaskTable(client_id,data) VALUES ('+IntToStr(ClientSocket.SocketHandle)',"'+incomingData+'")')
//IBQuery1.ExecSQL;
эту часть оставил в закомментированном виде, как пример записи в БД через стандартные средства

WriteLn и запись в БД внутри критической секции во избежание состояния гонки
