package com.example.app.dependency.bad;

public class NotificationService {
    private final MailService mailService;
    private final DatabaseService databaseService;

    public NotificationService() {
        this.mailService = new MailService();
        this.databaseService = new DatabaseService();
    }

    public void notifyUsers(String msg) {
        databaseService.connect();
        mailService.sendEmail(msg);
        databaseService.disconnect();
    }
}
