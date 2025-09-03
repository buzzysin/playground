package com.example.app.dependency;

import com.example.app.dependency.good.PhotoService;
import com.example.app.dependency.good.IFileService;
import com.example.app.dependency.good.IDatabaseService;

import org.junit.Test;

public class PhotoServiceTest {
    static class FakeFileService implements IFileService {
        public String lastPath;
        @Override
        public void storeFile(String path) { lastPath = path; }
    }

    static class FakeDatabase implements IDatabaseService {
        public boolean connected = false;
        @Override
        public void connect() { connected = true; }
        @Override
        public void disconnect() { connected = false; }
    }

    @Test
    public void testSavePhotoUsesFileAndDb() {
        FakeFileService ffs = new FakeFileService();
        FakeDatabase fdb = new FakeDatabase();
        PhotoService svc = new PhotoService(ffs, fdb);
        svc.savePhoto("/tmp/test.jpg");
        if (!"/tmp/test.jpg".equals(ffs.lastPath)) throw new AssertionError("file not stored");
        if (fdb.connected) throw new AssertionError("db should be disconnected after save");
    }
}
