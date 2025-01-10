import {Logger} from 'tslog';

const log = new Logger({
  name: 'websocket_typed',
  type: 'pretty',
});


// Type definitions for service metadata
interface ParamDefinition {
  name: string;
  type: string;
  optional?: boolean;
  default?: any;
}

interface MethodDefinition {
  name: string;
  parameters: readonly ParamDefinition[];  // Make readonly
  returnType: string;
}

interface ServiceDefinition {
  name: string;
  methods: {[key: string]: MethodDefinition;};
}

// Helper type for tuple to union conversion
type TupleToUnion<T extends readonly any[]> = T[number];

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

// Example service definition
const ApiServiceDefinition = {
  name: 'ApiService',
  methods: {
    getRoot: {name: 'getRoot', parameters: [], returnType: 'GetValueResponse'},
    setRootFile: {
      name: 'setRootFile',
      parameters: [{name: 'path', type: 'string'}],
      returnType: 'SetRootFileResponse'
    }
  }
} as const;

// Client implementation
function createWebSocketClient<T extends ServiceDefinition>(
    definition: T, ws: WebSocket): InferServiceClient<T> {
  // Use any temporarily to avoid index signature issue
  const client = {} as any;

  for (const [methodName, methodDef] of Object.entries(definition.methods)) {
    log.info(`${methodName}`)
    client[methodName] = async (params: any) => {
      return new Promise((resolve, reject) => {
        const id = Math.random().toString(36).substr(2, 9);

        // Convert params to expected format
        const args = {};
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
            ws.removeEventListener('message', handler);
            resolve(response);
          }
        };

        ws.addEventListener('message', handler);
      });
    };
  }

  return client as InferServiceClient<T>;
}

// Usage example
async function main() {
  const ws = new WebSocket('ws://localhost:8089');

  // Wait for connection
  await new Promise<void>(
      resolve => ws.addEventListener('open', () => resolve()));

  const client = createWebSocketClient(ApiServiceDefinition, ws);

  await client.setRootFile(
      {path: '/home/haxscramper/workspace/repos/fic/ordered.org'});

  const root = await client.getRoot({});
  log.info(`root: ${JSON.stringify(root)}`);
}

main();
