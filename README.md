# FDNS
DNS Server implementation in pure Haskell

## Test with wireshart

```bash
wireshark -i any -Y "udp.srcport == 9053 || udp.dstport == 9053 || udp.srcport == 53 || udp.dstport == 53" -k
```

```bash
dig google.com -p 9053 @127.0.0.1 +noedn
dig google.com +noedn
```
