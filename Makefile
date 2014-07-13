PROJECT     = Raytracer
MAIN        = Main
MAIN_MODULE = "${PROJECT}.${MAIN}"
EXEC        = rt
BUILD_DIR   = build

OPTIMIZED_SUBSET = \
	${PROJECT}/Utils/Array.hs \
	${PROJECT}/Utils/Math.hs \
	${PROJECT}/Space/Tensor.hs \
	${PROJECT}/Space/KDTree.hs \
	${PROJECT}/Utils/Array.hs \
	${PROJECT}/Surface/Texture.hs \
	${PROJECT}/Surface/Mapping.hs \
	${PROJECT}/Surface/Material.hs \
	${PROJECT}/World/Environment.hs \
	${PROJECT}/World/Color.hs \
	${PROJECT}/World/Lights.hs \
	${PROJECT}/World/Intersect.hs \
	${PROJECT}/World/AABB.hs \
	${PROJECT}/Objects/Attributes.hs \
	${PROJECT}/Objects/Primitive.hs \
	${PROJECT}/Objects/Object.hs \
	${PROJECT}/Render.hs \

CC = ghc
CC_FLAGS = \
	--make \
	-Wall \
	-rtsopts \
	-fwarn-tabs \
	-main-is ${MAIN_MODULE} \
	-odir="${BUILD_DIR}" \
	-hidir="${BUILD_DIR}" \

all:	basic

basic:	${PROJECT}/${MAIN}.hs
	$(CC) ${CC_FLAGS} -o "${EXEC}" -threaded  $^

optimized_llvm_subset:	${OPTIMIZED_SUBSET}
	$(CC) ${CC_FLAGS} -o "${EXEC}" \
		-threaded \
		-Odph \
		-fno-liberate-case \
		-funfolding-use-threshold1000 \
		-funfolding-keeness-factor1000 \
		-fllvm \
		-optlo-O3 \
		$^

optimized_asm_subset:	${OPTIMIZED_SUBSET}
	$(CC) ${CC_FLAGS} -o "${EXEC}" \
		-threaded \
		-Odph \
		-fno-liberate-case \
		-funfolding-use-threshold1000 \
		-funfolding-keeness-factor1000 \
		-fasm \
		-optlo-O3 \
		$^

optimized:	optimized_llvm_subset basic

profile:	${PROJECT}/${MAIN}.hs
	$(CC) ${CC_FLAGS} -o "${EXEC}-prof" \
	-osuf \
	-prof .p_o \
	-auto-all \
	-caf-all \
	-fforce-recomp \
	$^

clean:
	rm -f ${EXEC}
	rm -f ${EXEC}-prof
	rm -Rf ${BUILD_DIR}/*
	find . -name \*.o -type f -delete
	find "${BUILD_DIR}" -name \*.o -type f -delete
	find . -name \*.p_o -type f -delete
	find "${BUILD_DIR}" -name \*.p_o -type f -delete
	find . -name \*.hi -type f -delete
	find "${BUILD_DIR}" -name \*.hi -type f -delete
