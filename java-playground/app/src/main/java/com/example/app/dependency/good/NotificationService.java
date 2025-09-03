package com.example.app.dependency.good;

public class NotificationService {
    private final IMailService mailService;
    private final IDatabaseService databaseService;

    public NotificationService(IMailService mailService, IDatabaseService databaseService) {
        this.mailService = mailService;
        this.databaseService = databaseService;
    }

    public void notifyUsers(String msg) {
        databaseService.connect();
        mailService.sendEmail(msg);
        databaseService.disconnect();
    }
}
