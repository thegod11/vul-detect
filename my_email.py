import smtplib
from email.mime.text import MIMEText
from email.utils import formataddr

class EmailSender:
    def __init__(self, sender_email="1910738214@qq.com" , sender_auth_code="hvlhjtcrtslvejej", recipient_email="1910738214@qq.com", subject="vul-detect 运行结果通知"):
        """
        初始化邮件发送器
        :param sender_email: 发件人邮箱（QQ邮箱）
        :param sender_auth_code: 发件人授权码（QQ邮箱的SMTP授权码）
        :param recipient_email: 收件人邮箱（可以是字符串或列表）
        :param subject: 邮件主题（默认：无主题）
        """
        self.sender_email = sender_email
        self.sender_auth_code = sender_auth_code
        self.recipient_email = recipient_email
        self.subject = subject
        self.smtp_server = "smtp.qq.com"
        self.smtp_port = 465

    def send(self, content, content_type="plain"):
        """
        发送邮件
        :param content: 邮件正文内容
        :param content_type: 内容类型（plain/html）
        :return: 发送成功返回True，失败返回False
        """
        # 创建邮件对象
        msg = MIMEText(content, content_type, "utf-8")
        msg["Subject"] = self.subject
        msg["From"] = formataddr(("发件人", self.sender_email))  # 格式化发件人信息
        
        # 处理收件人格式（支持单个或多个收件人）
        if isinstance(self.recipient_email, list):
            msg["To"] = ", ".join(self.recipient_email)
        else:
            msg["To"] = self.recipient_email

        try:
            # 使用SSL连接SMTP服务器
            with smtplib.SMTP_SSL(self.smtp_server, self.smtp_port) as server:
                server.login(self.sender_email, self.sender_auth_code)
                server.sendmail(
                    self.sender_email,
                    self.recipient_email if isinstance(self.recipient_email, list) else [self.recipient_email],
                    msg.as_string()
                )
            return True
        except Exception as e:
            print(f"邮件发送失败: {str(e)}")
            return False

if __name__ == "__main__":
    # 使用示例
    sender = "1910738214@qq.com"        # 替换为你的QQ邮箱
    auth_code = "hvlhjtcrtslvejej"         # 替换为你的SMTP授权码
    recipient = "1910738214@qq.com"  # 替换为收件人邮箱
    
    mailer = EmailSender(
        sender_email=sender,
        sender_auth_code=auth_code,
        recipient_email=recipient,
        subject="测试邮件"
    )
    
    if mailer.send("这是一封来自Python的测试邮件"):
        print("邮件发送成功！")
    else:
        print("邮件发送失败！")