package com.example.app.dependency.bad;

public class MailService {
    private final DatabaseService databaseService;

    public MailService() {
        this.databaseService = new DatabaseService();
    }

    public void sendEmail(String msg) {
        System.out.println("[Bad] MailService: sending email: " + msg);
    }
}
