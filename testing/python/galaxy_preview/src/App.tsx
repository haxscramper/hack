import { Canvas } from '@react-three/fiber';
import { OrbitControls, Stars, Html } from '@react-three/drei';

export default function App() {
  return (
    <div style={{ width: '100vw', height: '100vh', background: 'black' }}>
      <Canvas>
        <ambientLight />
        <Stars />
        <OrbitControls />
        <mesh>
          <sphereGeometry args={[0.5, 32, 32]} />
          <meshBasicMaterial color="white" />
        </mesh>
      </Canvas>
    </div>
  );
}
