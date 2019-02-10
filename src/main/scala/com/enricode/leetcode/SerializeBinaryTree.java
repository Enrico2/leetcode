package com.enricode.leetcode;

import com.enricode.util.TreeNode;

/**
 * https://leetcode.com/problems/serialize-and-deserialize-binary-tree/description/
 */
public class SerializeBinaryTree {

    public static class Codec {

        // Encodes a tree to a single string.
        public String serialize(TreeNode root) {
            if (root == null) return "(null)";

            StringBuilder sb = new StringBuilder();
            sb.append("(");
            sb.append(root.val);
            sb.append(serialize(root.left));
            sb.append(serialize(root.right));
            sb.append(")");

            return sb.toString();
        }


        private TreeNode deserialize(String data, int i, int j) {
            if (data.substring(i, j).equals("(null)")) return null;

            if (data.charAt(i) == '(') {
                int k = i;
                while (data.charAt(++k) != '(') {
                }
                int val = Integer.parseInt(data.substring(i + 1, k));

                int stack = 1;
                int l = k;
                while (stack > 0) {
                    l++;
                    if (data.charAt(l) == '(') stack++;
                    if (data.charAt(l) == ')') stack--;
                }

                TreeNode left = deserialize(data, k, l + 1);

                k = l + 1;
                stack++;
                l = k;
                while (stack > 0) {
                    l++;
                    if (data.charAt(l) == '(') stack++;
                    if (data.charAt(l) == ')') stack--;
                }

                TreeNode right = deserialize(data, k, l + 1);

                if (data.charAt(l + 1) != ')') {
                    System.out.println("problem with String end: " + data);
                    return null;
                }

                return new TreeNode(val, left, right);

            } else {
                System.out.println("problem with String start: " + data);
                return null;
            }
        }

        // Decodes your encoded data to tree.
        public TreeNode deserialize(String data) {
            if (data == null || data.trim().equals("")) return null;
            return deserialize(data, 0, data.length());
        }
    }


    public static void main(String[] args) {
        TreeNode root = new TreeNode(1, new TreeNode(2), new TreeNode(3, new TreeNode(4), new TreeNode(5)));
        Codec c = new Codec();
        String str = c.serialize(root);
        System.out.println(str);
        TreeNode back = c.deserialize(str);
        System.out.println(back.equals(root));
    }
}
