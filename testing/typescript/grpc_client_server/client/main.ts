import {credentials} from '@grpc/grpc-js';
import {GrpcTransport} from '@protobuf-ts/grpc-transport';
import {Client, createChannel, createClient} from 'nice-grpc';
import {match, P} from 'ts-pattern';

import * as org from './generated/org.ts';



async function treeRepr(
    client: Client<typeof org.OrgServiceDefinition>, handle: number,
    depth: number) {
  const kind: org.KindResponse = await client.getKind({handle: handle});
  const size: org.CountResponse =
      await client.getSubnodeCount({handle: handle});
  const kind_str = org.kindResponse_KindToJSON(kind.kind);
  console.log(
      ' '.repeat(depth), kind_str, size.count,
      match({kind: kind_str})
          .with(
              {kind: 'TEXT'},
              async function() {
                return client.getText({handle: handle});
              })
          .with({}, () => '')
          .state.value);

  for (var i = 0; i < size.count; i++) {
    const sub: org.HandleRequest =
        await client.getSubnode({handle: handle, index: i});
    treeRepr(client, sub.handle, depth + 1);
  }
}

async function example() {
  const channel = createChannel('localhost:50051');

  const client: Client<typeof org.OrgServiceDefinition> =
      createClient(org.OrgServiceDefinition, channel);

  const response: org.HandleResponse = await client.getRoot({});
  console.log('Got response:', response, 'handle field is', response.handle);
  console.log(
      'Response handle kind is',
      await client.getKind({handle: response.handle}));

  treeRepr(client, response.handle, 0);
}

example().catch(console.error);
