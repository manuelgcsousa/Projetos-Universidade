all:
		g++ -o generator generator.cpp
		@echo "\n"
		@echo "=> INSTRUCTIONS <="
		@echo "./generator plane <length> plane.3d"
		@echo "./generator box <length> <width> <height> (div) box.3d"
		@echo "./generator sphere <radius> <slices> <stacks> sphere.3d"
		@echo "./generator cone <radius> <height> <slices> <stacks> cone.3d"
		@echo "./generator diamond <radius> diamond.3d"
		@echo "./generator cylinder <radius> <height> <slices> <stacks> cylinder.3d"
		@echo "\n"
clean:
	rm generator
	rm ./objects/*.3d
