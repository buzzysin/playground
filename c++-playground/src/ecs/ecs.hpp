#pragma once
#include <cstdint>
#include <vector>
#include <unordered_map>
#include <bitset>
#include <iostream>
#include <string>

// Minimal ECS demonstration for presentation purposes.

using Entity = uint32_t;
constexpr Entity INVALID_ENTITY = 0;

struct Transform {
  float x{0}, y{0};
  float rotation{0};
};

struct Renderable {
  std::string mesh;
};

struct Physics {
  float vx{0}, vy{0};
  float mass{1.0f};
};

// Simple sparse-set implementation (entity -> dense index mapping)
template<typename T>
class SparseSet {
public:
  void insert(Entity e, T value) {
    if (e >= sparse.size()) sparse.resize(e + 1, INVALID_INDEX);
    if (sparse[e] != INVALID_INDEX) {
      dense[sparse[e]] = value;
      return;
    }
    sparse[e] = dense.size();
    dense.push_back(value);
    entities.push_back(e);
  }

  bool contains(Entity e) const {
    return e < sparse.size() && sparse[e] != INVALID_INDEX;
  }

  T& get(Entity e) {
    return dense[sparse[e]];
  }

  const std::vector<Entity>& getEntities() const { return entities; }

  size_t size() const { return dense.size(); }

private:
  static constexpr int INVALID_INDEX = -1;
  std::vector<int> sparse;
  std::vector<T> dense;
  std::vector<Entity> entities;
};

class World {
public:
  Entity create() {
    Entity id = ++m_lastId;
    m_entities.push_back(id);
    return id;
  }

  void addTransform(Entity e, Transform t) { m_transforms.insert(e, t); }
  void addRenderable(Entity e, Renderable r) { m_renderables.insert(e, r); }
  void addPhysics(Entity e, Physics p) { m_physics.insert(e, p); }

  bool hasTransform(Entity e) const { return m_transforms.contains(e); }
  bool hasRenderable(Entity e) const { return m_renderables.contains(e); }
  bool hasPhysics(Entity e) const { return m_physics.contains(e); }
  Transform& transform(Entity e) { return m_transforms.get(e); }
  Renderable& renderable(Entity e) { return m_renderables.get(e); }
  Physics& physics(Entity e) { return m_physics.get(e); }

  const std::vector<Entity>& entities() const { return m_entities; }
  const std::vector<Entity>& transformEntities() const { return m_transforms.getEntities(); }
  const std::vector<Entity>& renderEntities() const { return m_renderables.getEntities(); }
  const std::vector<Entity>& physicsEntities() const { return m_physics.getEntities(); }

  // Helpers for demo
  Entity createPlayer(float x, float y) {
    auto e = create();
    addTransform(e, Transform{x,y,0});
    addRenderable(e, Renderable{"player.mesh"});
    addPhysics(e, Physics{1.0f, 0.0f, 1.0f});
    return e;
  }

  Entity createTree(float x, float y) {
    auto e = create();
    addTransform(e, Transform{x,y,0});
    addRenderable(e, Renderable{"tree.mesh"});
    return e;
  }

  // Query entities that have both Transform and Renderable efficiently by iterating the smaller dense set
  std::vector<Entity> queryTransformAndRenderable() const {
    const auto &tents = m_transforms.getEntities();
    const auto &rents = m_renderables.getEntities();
    const auto &small = (tents.size() < rents.size()) ? tents : rents;
    std::vector<Entity> out; out.reserve(std::min(tents.size(), rents.size()));
    for (auto e : small) {
      if (m_transforms.contains(e) && m_renderables.contains(e)) out.push_back(e);
    }
    return out;
  }

private:
  Entity m_lastId{0};
  std::vector<Entity> m_entities;
  SparseSet<Transform> m_transforms;
  SparseSet<Renderable> m_renderables;
  SparseSet<Physics> m_physics;
};

// Simple systems
struct PhysicsSystem {
  void update(World& w, float dt) {
    for (auto e : w.entities()) {
      if (w.hasPhysics(e) && w.hasTransform(e)) {
        auto &p = w.physics(e);
        auto &t = w.transform(e);
        t.x += p.vx * dt;
        t.y += p.vy * dt;
      }
    }
  }
};

struct RenderSystem {
  void update(World& w) {
    std::cout << "-- Render Pass --\n";
    auto list = w.queryTransformAndRenderable();
    for (auto e : list) {
      auto &r = w.renderable(e);
      auto &t = w.transform(e);
      std::cout << "Entity " << e << " | pos=(" << t.x << ", " << t.y << ") | mesh=\"" << r.mesh << "\"\n";
    }
  }
};
