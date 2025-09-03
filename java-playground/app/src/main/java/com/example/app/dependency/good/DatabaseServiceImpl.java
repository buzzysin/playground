package com.example.app.dependency.good;

public class DatabaseServiceImpl implements IDatabaseService {
    @Override
    public void connect() {
        System.out.println("[Good] DatabaseServiceImpl: connect");
    }

    @Override
    public void disconnect() {
        System.out.println("[Good] DatabaseServiceImpl: disconnect");
    }
}
