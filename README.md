peer-to-peer IRC-inspired self-hosted web chat.

### Self-hosted install

 1. Download a release.
 2. Upload it to your static web host.

### Configure STUN/TURN servers

For better performance behind restrictive firewalls you may wish to specify your own STUN/TURN webRTC servers. You can do that in the console as follows:

```
	localStorage["dirc-wt-config"] = JSON.stringify({rtcConfig: {tracker: {iceServers: [{urls: 'YOUR-SERVER' }, {url: 'YOUR-OTHER-SERVER'}, ...] }}});
```

### Limitations

 * Peer discovery uses [webtorrent](https://webtorrent.io/) wss trackers - essentially a centralized service.
 * No security audit has been done on this codebase.
 * Web browsers are notoriously leaky.
 * WebRTC has been known to leak around Tor.
 * Eavesdropping on a channel without revealing listener is easy.

In short, this is an experiment, please don't rely on it for high security communication.

Patches welcome!
