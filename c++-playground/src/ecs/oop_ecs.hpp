#pragma once
#include <iostream>
#include <memory>
#include <unordered_map>
#include <typeindex>
#include <string>
#include <vector>
#include <stdexcept>

namespace oop {

class ComponentInterface {
public:
    virtual ~ComponentInterface() = default;
    virtual void update() = 0;
    virtual std::type_index getType() const = 0;
};

struct TransformComponent : ComponentInterface {
    TransformComponent(float x=0,float y=0,float z=0):x(x),y(y),z(z){}
    void update() override { /* demo */ }
    std::type_index getType() const override { return typeid(TransformComponent); }
    float x,y,z;
};

struct RenderComponent : ComponentInterface {
    RenderComponent(std::string m="", std::string t=""):mesh(std::move(m)),texture(std::move(t)){}
    void update() override { /* demo */ }
    std::type_index getType() const override { return typeid(RenderComponent); }
    std::string mesh, texture;
};

class Component {
public:
    Component() = default;
    ~Component() = default;

    void update() { if (impl) impl->update(); }

    template<typename T, typename... Args>
    void create(Args&&... args) {
        if (impl) throw std::runtime_error("component already created");
        impl = std::make_unique<Model<T>>(std::forward<Args>(args)...);
    }

    template<typename T>
    T* as() {
        if (!impl) return nullptr;
        if (impl->getType() != typeid(T)) return nullptr;
        return &static_cast<Model<T>*>(impl.get())->data;
    }

    std::type_index typeIndex() const { return impl ? impl->getType() : std::type_index(typeid(void)); }

private:
    struct Concept { virtual ~Concept() = default; virtual void update() = 0; virtual std::type_index getType() const = 0; };
    template<typename T>
    struct Model : Concept { 
        template<typename... Args> Model(Args&&... args): data(std::forward<Args>(args)...) {}
        void update() override { data.update(); }
        std::type_index getType() const override { return typeid(T); }
        T data;
    };
    std::unique_ptr<Concept> impl;
};

class Entity {
public:
    explicit Entity(int id): id(id) {}
    int id;
    std::unordered_map<std::type_index, Component> components;

    template<typename T, typename... Args>
    void addComponent(Args&&... args) {
        auto &c = components[typeid(T)];
        c.create<T>(std::forward<Args>(args)...);
    }

    template<typename T>
    T* getComponent() {
        auto it = components.find(typeid(T));
        if (it == components.end()) return nullptr;
        return it->second.as<T>();
    }

    void updateComponents() {
        for (auto &kv : components) kv.second.update();
    }
};

} // namespace oop
