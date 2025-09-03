package com.example.app.dependency.bad;

public class DatabaseService {
    public void connect() {
        System.out.println("[Bad] DatabaseService: connect");
    }

    public void disconnect() {
        System.out.println("[Bad] DatabaseService: disconnect");
    }
}
