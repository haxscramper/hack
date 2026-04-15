import { Hono } from 'hono';
import { serveStatic } from '@hono/node-server/serve-static';
import fs from 'node:fs/promises';
import path from 'node:path';

const app = new Hono();
const CONFIG_PATH = path.join(process.cwd(), 'config.json');

app.get('/api/config', async (c) => {
  try {
    const data = await fs.readFile(CONFIG_PATH, 'utf-8');
    return c.json(JSON.parse(data));
  } catch (e) {
    return c.json({ error: 'Config not found' }, 404);
  }
});

app.post('/api/config', async (c) => {
  const body = await c.req.json();
  await fs.writeFile(CONFIG_PATH, JSON.stringify(body, null, 2));
  return c.json({ success: true });
});

app.get('/*', serveStatic({ root: './dist' }));

export default {
  port: 3000,
  fetch: app.fetch,
};
