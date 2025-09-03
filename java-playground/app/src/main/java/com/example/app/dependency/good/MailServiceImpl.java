package com.example.app.dependency.good;

public class MailServiceImpl implements IMailService {
    @Override
    public void sendEmail(String msg) {
        System.out.println("[Good] MailServiceImpl: sending email: " + msg);
    }
}
