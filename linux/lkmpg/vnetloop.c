// SPDX-License-Identifier: GPL-2.0
/*
 * vnetloop.c - Minimal virtual Ethernet device that loops transmitted packets
 * back into the receive path.
 */

#include <linux/etherdevice.h>
#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>
#include <linux/u64_stats_sync.h>

struct vnetloop_priv {
    u64 tx_packets;
    u64 tx_bytes;
    u64 rx_packets;
    u64 rx_bytes;
    struct u64_stats_sync syncp;
};

static netdev_tx_t vnetloop_xmit(struct sk_buff *skb, struct net_device *dev) {
    struct vnetloop_priv *priv = netdev_priv(dev);
    struct sk_buff *rx_skb;
    unsigned int len = skb->len;

    rx_skb = skb_copy(skb, GFP_ATOMIC);
    if (rx_skb) {
        rx_skb->dev = dev;
        skb_reset_mac_header(rx_skb);
        rx_skb->ip_summed = CHECKSUM_UNNECESSARY;
        rx_skb->protocol = eth_type_trans(rx_skb, dev);

        u64_stats_update_begin(&priv->syncp);
        priv->tx_packets++;
        priv->tx_bytes += len;
        priv->rx_packets++;
        priv->rx_bytes += rx_skb->len;
        u64_stats_update_end(&priv->syncp);

        netif_rx(rx_skb);
    } else {
        u64_stats_update_begin(&priv->syncp);
        priv->tx_packets++;
        priv->tx_bytes += len;
        u64_stats_update_end(&priv->syncp);
    }

    dev_kfree_skb(skb);

    return NETDEV_TX_OK;
}

static int vnetloop_open(struct net_device *dev) {
    netif_carrier_on(dev);
    netif_start_queue(dev);

    return 0;
}

static int vnetloop_stop(struct net_device *dev) {
    netif_stop_queue(dev);
    netif_carrier_off(dev);

    return 0;
}

static void vnetloop_get_stats64(struct net_device *dev,
                                 struct rtnl_link_stats64 *stats) {
    struct vnetloop_priv *priv = netdev_priv(dev);
    unsigned int start;

    do {
        start = u64_stats_fetch_begin(&priv->syncp);
        stats->tx_packets = priv->tx_packets;
        stats->tx_bytes = priv->tx_bytes;
        stats->rx_packets = priv->rx_packets;
        stats->rx_bytes = priv->rx_bytes;
    } while (u64_stats_fetch_retry(&priv->syncp, start));
}

static const struct net_device_ops vnetloop_netdev_ops = {
    .ndo_open = vnetloop_open,
    .ndo_stop = vnetloop_stop,
    .ndo_start_xmit = vnetloop_xmit,
    .ndo_get_stats64 = vnetloop_get_stats64,
};

static void vnetloop_setup(struct net_device *dev) {
    dev->netdev_ops = &vnetloop_netdev_ops;
    dev->flags |= IFF_NOARP;
    dev->features |= NETIF_F_HW_CSUM;
    eth_hw_addr_random(dev);
}

static struct net_device *vnetloop_dev;

static int __init vnetloop_init(void) {
    int ret;
    vnetloop_dev = alloc_etherdev(sizeof(struct vnetloop_priv));
    if (!vnetloop_dev) {
        return -ENOMEM;
    }

    strscpy(vnetloop_dev->name, "vnetloop%d", IFNAMSIZ);
    vnetloop_setup(vnetloop_dev);

    ret = register_netdev(vnetloop_dev);
    if (ret) {
        free_netdev(vnetloop_dev);
        return ret;
    }

    pr_info("vnetloop: registered %s\n", vnetloop_dev->name);

    return 0;
}

static void __exit vnetloop_exit(void) { return; }

module_init(vnetloop_init);
module_exit(vnetloop_exit);

MODULE_DESCRIPTION("LKMPG virtual net_device example");
MODULE_LICENSE("GPL");
