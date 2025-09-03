package com.example.app.dependency.good;

public class PhotoService {
    private final IFileService fileService;
    private final IDatabaseService databaseService;

    public PhotoService(IFileService fileService, IDatabaseService databaseService) {
        this.fileService = fileService;
        this.databaseService = databaseService;
    }

    public void savePhoto(String path) {
        databaseService.connect();
        fileService.storeFile(path);
        databaseService.disconnect();
    }
}
