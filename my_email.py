import smtplib
import threading
from email.mime.text import MIMEText
from email.utils import formataddr
from datetime import datetime

class EmailSender:
    def __init__(self, 
                 sender_email="1910738214@qq.com",
                 sender_auth_code="hvlhjtcrtslvejej",
                 recipient_email="1910738214@qq.com",
                 subject="vul-detect 运行结果通知",
                 heartbeat_callback=None,
                 heartbeat_interval=10):
        """
        初始化邮件发送器
        :param heartbeat_callback: 心跳信息生成函数（返回字符串）
        :param heartbeat_interval: 心跳间隔秒数（默认半小时）
        """
        # 邮件配置
        self.sender_email = sender_email
        self.sender_auth_code = sender_auth_code
        self.recipient_email = recipient_email
        self.subject = subject
        self.smtp_server = "smtp.qq.com"
        self.smtp_port = 465
        
        # 心跳模块
        self.heartbeat_callback = heartbeat_callback
        self.heartbeat_interval = heartbeat_interval
        self._timer = None
        
        # 自动启动心跳监测
        if self.heartbeat_callback:
            self._schedule_heartbeat()

    def _schedule_heartbeat(self):
        """调度下一次心跳发送"""
        self._timer = threading.Timer(self.heartbeat_interval, self._send_heartbeat)
        self._timer.daemon = True  # 设置为守护线程
        self._timer.start()

    def _send_heartbeat(self):
        """执行心跳发送"""
        try:
            content = self.heartbeat_callback()
            self.send(f"[心跳监测] {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n{content}")
        except Exception as e:
            print(f"心跳发送失败: {str(e)}")
        finally:
            self._schedule_heartbeat()  # 重新调度

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

    def stop_heartbeat(self):
        """停止心跳监测"""
        if self._timer:
            self._timer.cancel()

if __name__ == "__main__":
    # 示例心跳回调函数
    def custom_heartbeat():
        return "系统运行状态：正常\n检测服务存活：是\n待处理任务：0"

    # 初始化邮件发送器（带心跳）
    mailer = EmailSender(
        sender_email="1910738214@qq.com",
        sender_auth_code="hvlhjtcrtslvejej",
        recipient_email="1910738214@qq.com",
        subject="服务状态监测",
        heartbeat_callback=custom_heartbeat,
        heartbeat_interval=10  # 测试用60秒间隔
    )

    try:
        # 保持主线程运行
        while True:
            pass
    except KeyboardInterrupt:
        mailer.stop_heartbeat()
        print("已停止心跳监测")