package com.example.app.dependency.bad;

public class PhoneService {
    private final DatabaseService databaseService;

    public PhoneService() {
        this.databaseService = new DatabaseService();
    }

    public void sendSMS(String msg) {
        System.out.println("[Bad] PhoneService: sending SMS: " + msg);
    }
}
