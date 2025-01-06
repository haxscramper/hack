import * as org from "./generated/org.ts";
import { GrpcTransport } from "@protobuf-ts/grpc-transport";
import { credentials } from '@grpc/grpc-js';
import {createChannel, createClient, Client} from 'nice-grpc';

async function example() {
  const channel = createChannel('localhost:50051');

  const client: Client<typeof org.OrgServiceDefinition> = createClient(
    org.OrgServiceDefinition,
    channel
  );

  const response = await client.getRoot({});
  console.log("Got response:", response, "handle field is", response.handle);
}

example().catch(console.error);
