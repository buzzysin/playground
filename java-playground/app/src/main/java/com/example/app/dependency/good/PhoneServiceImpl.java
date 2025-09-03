package com.example.app.dependency.good;

public class PhoneServiceImpl implements IPhoneService {
    @Override
    public void sendSMS(String msg) {
        System.out.println("[Good] PhoneServiceImpl: sending SMS: " + msg);
    }
}
