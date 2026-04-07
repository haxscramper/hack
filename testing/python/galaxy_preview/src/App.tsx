import { useState, useEffect } from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls, Stars, Html } from '@react-three/drei';

interface Location {
  ra: { h: number; m: number; s: number };
  dec: { deg: number; m: number; s: number };
  distance: number;
}

interface CelestialObject {
  id: string;
  name: string;
  location: Location;
}

interface Config {
  celestialObjects: CelestialObject[];
}

function raDecToCartesian(location: any) {
  const ra = location.ra || {};
  const dec = location.dec || {};
  const distance = location.distance || 0;
  const raDeg = ((ra.h || 0) + (ra.m || 0) / 60 + (ra.s || 0) / 3600) * 15;
  const decDeg = Math.abs(dec.deg || 0) + (dec.m || 0) / 60 + (dec.s || 0) / 3600;
  const decSign = dec.deg < 0 ? -1 : 1;
  const finalDecDeg = decDeg * decSign;

  const raRad = raDeg * (Math.PI / 180);
  const decRad = finalDecDeg * (Math.PI / 180);

  const x = distance * Math.cos(decRad) * Math.cos(raRad);
  const y = distance * Math.cos(decRad) * Math.sin(raRad);
  const z = distance * Math.sin(decRad);

  return [x, y, z] as [number, number, number];
}

export default function App() {
  const [config, setConfig] = useState<Config | null>(null);

  useEffect(() => {
    fetch('/api/config')
      .then((res) => res.json())
      .then((data) => setConfig(data as Config))
      .catch((err) => console.error('Error fetching config:', err));
  }, []);

  if (!config) return null;

  return (
    <div style={{ width: '100vw', height: '100vh', background: 'black' }}>
      <Canvas camera={{ position: [0, 0, 15] }}>
        <ambientLight />
        <OrbitControls />
        {config.celestialObjects.map((obj) => {
          const [x, y, z] = raDecToCartesian(obj.location);
          return (
            <mesh key={obj.id} position={[x, y, z]}>
              <sphereGeometry args={[0.2, 32, 32]} />
              <meshBasicMaterial color="white" />
              <Html distanceFactor={10}>
                <div style={{ color: 'white', whiteSpace: 'nowrap' }}>{obj.name}</div>
              </Html>
            </mesh>
          );
        })}
      </Canvas>
    </div>
  );
}
