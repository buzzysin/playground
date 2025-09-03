package com.example.app.dependency;

import com.example.app.dependency.good.Container;

public class DependencyDemo {
    public static void main(String[] args) {
        System.out.println("--- Bad example (tight coupling) ---");
        com.example.app.dependency.bad.PhotoService badPhoto = new com.example.app.dependency.bad.PhotoService();
        badPhoto.savePhoto("/tmp/bad.jpg");

        System.out.println("\n--- Good example (DI) ---");
        Container container = new Container();
        com.example.app.dependency.good.PhotoService photo = container.resolvePhotoService();
        photo.savePhoto("/tmp/good.jpg");

        com.example.app.dependency.good.NotificationService notify = container.resolveNotificationService();
        notify.notifyUsers("Hello users");
    }
}
