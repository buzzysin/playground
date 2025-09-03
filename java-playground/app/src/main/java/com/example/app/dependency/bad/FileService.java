package com.example.app.dependency.bad;

public class FileService {
    public void storeFile(String path) {
        System.out.println("[Bad] FileService: storing " + path);
    }
}
