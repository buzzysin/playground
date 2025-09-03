package com.example.app.dependency.bad;

public class PhotoService {
    private final FileService fileService;
    private final DatabaseService databaseService;

    public PhotoService() {
        // bad: constructs concrete dependencies itself
        this.fileService = new FileService();
        this.databaseService = new DatabaseService();
    }

    public void savePhoto(String path) {
        databaseService.connect();
        fileService.storeFile(path);
        databaseService.disconnect();
    }
}
