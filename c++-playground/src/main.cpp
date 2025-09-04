// Simple demo: create a World, add entities/components, run a few update cycles.
#include "ecs/ecs.hpp"
#include <iostream>

int main() {
	World world;

	auto e1 = world.createPlayer(0.0f, 0.0f);
	auto e2 = world.createTree(5.0f, 5.0f);

	PhysicsSystem phys;
	RenderSystem render;

	const float dt = 1.0f;
	for (int frame = 0; frame < 3; ++frame) {
		std::cout << "--- Frame " << frame << " ---\n";
		phys.update(world, dt);
    render.update(world);

    // Show quick query result
    auto visible = world.queryTransformAndRenderable();
    std::cout << "Visible entities: ";
    for (auto e : visible) std::cout << e << " ";
    std::cout << "\n";
	}

	return 0;
}
