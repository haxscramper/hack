import {Logger} from 'tslog';

const log = new Logger({
  name: 'websocket_typed',
  type: 'pretty',
});


// Type definitions for service metadata
interface ParamDefinition {
  readonly name: string;
  readonly type: string;
  readonly optional?: boolean;
  readonly default?: any;
}

interface MethodDefinition {
  readonly name: string;
  readonly parameters: readonly ParamDefinition[];
  readonly result: string;
}

interface ServiceDefinition {
  readonly name: string;
  readonly methods: {[key: string]: MethodDefinition;};
}

// Helper type for tuple to union conversion
type TupleToUnion<T extends readonly any[]> = T[number];

interface ImmUniqId {}


// Improved type inference helpers
type InferParamType<T extends string> =
    T extends 'string' ? string : T extends 'number' ? number : never;

type InferMethodParams<T extends MethodDefinition> = {
  [P in TupleToUnion<T['parameters']>as P['name']]: P['optional'] extends true ?
      InferParamType<P['type']>|undefined :
      InferParamType<P['type']>
};

type InferServiceClient<T extends ServiceDefinition> = {
  [M in keyof T['methods']]: (params: InferMethodParams<T['methods'][M]>) =>
      Promise<any>
};

import {data} from '/tmp/schema.ts';

// Example service definition
const OrgServiceDefinition = data;

type OrgClient = InferServiceClient<typeof OrgServiceDefinition>;

// Client implementation
function createWebSocketClient(ws: WebSocket): OrgClient {
  // Use any temporarily to avoid index signature issue
  const client = {} as any;

  const definition = OrgServiceDefinition as ServiceDefinition;

  for (const [methodName, methodDef] of Object.entries(definition.methods)) {
    client[methodName] = async (params: any) => {
      return new Promise((resolve, reject) => {
        const id = Math.random().toString(36).substr(2, 9);

        // Convert params to expected format
        const args = {};
        // log.info(`TS params ${JSON.stringify(params)}`)
        for (const param of methodDef.parameters) {
          if (params[param.name] !== undefined) {
            args[param.name] = params[param.name];
          } else if ('default' in param) {
            args[param.name] = param.default;
          } else if (!param.optional) {
            throw new Error(`Missing required parameter: ${param.name}`);
          }
        }

        // Send request
        ws.send(
            JSON.stringify({target: `/api/${methodName}`, id: id, body: args}));

        // Handle response
        const handler = (event: MessageEvent) => {
          const response = JSON.parse(event.data);
          if (response.id === id) {
            log.info(`ID: ${id} got response: ${JSON.stringify(response)}`)
            ws.removeEventListener('message', handler);
            resolve(response.body);
          }
        };

        ws.addEventListener('message', handler);
      });
    };
  }

  return client as OrgClient;
}

async function treeRepr(client: OrgClient, id: ImmUniqId, depth: number = 0) {
  log.info(`${JSON.stringify(await client.getKind({id: id}))}`)
}

// Usage example
async function main() {
  const ws = new WebSocket('ws://localhost:8089');

  // Wait for connection
  await new Promise<void>(
      resolve => ws.addEventListener('open', () => resolve()));

  const client = createWebSocketClient(ws);
  client.setExceptionHandler({handler: true});

  await client.setRootFile(
      {path: '/home/haxscramper/workspace/repos/fic/ordered.org'});

  const root = await client.getRoot({});
  log.info(`root: ${JSON.stringify(root)}`);

  treeRepr(client, root, 0); 
}

main();
