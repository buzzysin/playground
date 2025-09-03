package com.example.app.dependency.good;

public class FileServiceImpl implements IFileService {
    @Override
    public void storeFile(String path) {
        System.out.println("[Good] FileServiceImpl: storing " + path);
    }
}
