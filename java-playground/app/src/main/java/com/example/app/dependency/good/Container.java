package com.example.app.dependency.good;

public class Container {
    public PhotoService resolvePhotoService() {
        IDatabaseService db = new DatabaseServiceImpl();
        IFileService file = new FileServiceImpl();
        return new PhotoService(file, db);
    }

    public NotificationService resolveNotificationService() {
        IDatabaseService db = new DatabaseServiceImpl();
        IMailService mail = new MailServiceImpl();
        return new NotificationService(mail, db);
    }
}
